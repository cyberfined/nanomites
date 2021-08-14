[Syscall("write", [ArgType.INT, ArgType.ADDR, ArgType.INT], ArgType.INT),
 Syscall("read",  [ArgType.INT, ArgType.ADDR, ArgType.INT], ArgType.INT),
 Syscall("exit", [ArgType.INT], ArgType.INT),
 Proc("someproc", [ArgType.INT], ArgType.INT)
]
