(** Generates bytecode from {!Core}.

    Closure conversion happens here. Each lambda gets its own code object, and its free
    variables are copied into the closure when it is created ("flat closures"). This is
    safe because values never change after being bound. The one exception is [letrec],
    where closures that refer to each other are completed in a second step. *)

val program : Core.expr -> Bytecode.code
(** Compiles a unit's entry point. Each function is run through
    {!Bytecode.max_operand_depth}, so a code generation bug raises [Failure] here instead
    of corrupting the VM's stack. *)
