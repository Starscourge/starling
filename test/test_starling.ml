let () =
  Alcotest.run "starling"
    [ ("bigint", Test_bigint.suite); ("frontend", Test_frontend.suite) ]
