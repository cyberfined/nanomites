[Syscall("write", [ArgType.INT, ArgType.ADDR, ArgType.INT], ArgType.INT),
 Syscall("read",  [ArgType.INT, ArgType.ADDR, ArgType.INT], ArgType.INT),
 Syscall("exit", [ArgType.INT], ArgType.INT),
 Syscall("clock_gettime", [ArgType.INT, ArgType.ADDR], ArgType.INT),
 Proc("memcpy", [ArgType.ADDR, ArgType.ADDR, ArgType.SIZE_T], ArgType.ADDR),
 Proc("memmove", [ArgType.ADDR, ArgType.ADDR, ArgType.SIZE_T], ArgType.ADDR),
 Proc("itoa", [ArgType.SIZE_T, ArgType.ADDR], ArgType.SIZE_T),
]
