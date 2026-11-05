(* Run with: dune exec --profile release bench/bench.exe

   The release profile matters, because the default dev profile turns off inlining across
   modules. Each result is the median of several runs. *)

open Starling

let runs = 7

let time f =
  let start = Unix.gettimeofday () in
  ignore (Sys.opaque_identity (f ()));
  (Unix.gettimeofday () -. start) *. 1000.

let median_ms f =
  let times = List.init runs (fun _ -> time f) |> List.sort Float.compare in
  List.nth times (runs / 2)

let row name ms = Printf.printf "| %-36s | %10.2f |\n%!" name ms

let header title column =
  Printf.printf "\n%s\n\n| %-36s | %10s |\n|%s|%s|\n" title "Benchmark" column
    (String.make 38 '-') (String.make 12 '-')

(* Programs on the VM. Definitions load once, and only the final expression gets timed. *)

let definitions =
  {|
(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(define (loop n acc) (if (= n 0) acc (loop (- n 1) (+ acc 1))))
(define (factorial n) (if (= n 0) 1 (* n (factorial (- n 1)))))

(define (safe? column placed)
  (define (check rest distance)
    (match rest
      (nil true)
      ((cons other more)
       (and (!= other column)
            (!= (abs (- other column)) distance)
            (check more (+ distance 1))))))
  (check placed 1))

(define (solutions n row)
  (if (= row 0)
      (list nil)
      (concat-map
       (lambda (placed)
         (map (lambda (column) (cons column placed))
              (filter (lambda (column) (safe? column placed)) (range 0 n))))
       (solutions n (- row 1)))))

;; A linear congruential generator, so the input to sort is the same every run.
(define (random-list n seed)
  (define (go k x acc)
    (if (= k 0) acc (go (- k 1) (rem (+ (* x 1103515245) 12345) 2147483648) (cons x acc))))
  (go n seed nil))

(define shuffled (random-list 200000 42))
(define numbers (range 0 1000000))
|}

let vm_benchmarks =
  [
    ("fib 32 (function calls)", "(fib 32)");
    ("tail-recursive loop, 10M iterations", "(loop 10000000 0)");
    ( "map, filter, sum over 1M elements",
      "(sum (map (lambda (x) (* x 2)) (filter even? numbers)))" );
    ("sort 200k integers", "(sort < shuffled)");
    ("9-queens, all solutions", "(length (solutions 9 9))");
    ("2000! through the VM", "(factorial 2000)");
  ]

let bench_vm () =
  header "Programs on the VM" "ms";
  let session = Driver.create ~out:ignore () in
  ignore (Driver.eval session { name = "bench"; text = definitions });
  List.iter
    (fun (name, text) ->
      row name (median_ms (fun () -> Driver.eval session { name = "bench"; text })))
    vm_benchmarks

(* Starling's Bigint against Zarith, which uses GMP. *)

let factorial_big n =
  let r = ref Bigint.one in
  for i = 2 to n do
    r := Bigint.mul !r (Bigint.of_int i)
  done;
  !r

let factorial_z n =
  let r = ref Z.one in
  for i = 2 to n do
    r := Z.mul !r (Z.of_int i)
  done;
  !r

let bench_bigint () =
  Printf.printf "\nBigint against Zarith\n\n";
  Printf.printf "| %-36s | %10s | %10s |\n" "Benchmark" "Bigint ms" "Zarith ms";
  Printf.printf "|%s|%s|%s|\n" (String.make 38 '-') (String.make 12 '-')
    (String.make 12 '-');
  let compare name f_big f_z =
    Printf.printf "| %-36s | %10.3f | %10.3f |\n%!" name (median_ms f_big) (median_ms f_z)
  in
  let big = factorial_big 20000 and z = factorial_z 20000 in
  let big_square = Bigint.mul big big and z_square = Z.mul z z in
  compare "2000! by repeated multiplication"
    (fun () -> factorial_big 2000)
    (fun () -> factorial_z 2000);
  compare "square a 77,000-digit number"
    (fun () -> Bigint.mul big big)
    (fun () -> Z.mul z z);
  compare "divide 154,000 by 77,000 digits"
    (fun () -> Bigint.quo big_square big)
    (fun () -> Z.div z_square z);
  compare "print a 154,000-digit number"
    (fun () -> Bigint.to_string big_square)
    (fun () -> Z.to_string z_square)

let () =
  bench_vm ();
  bench_bigint ()
