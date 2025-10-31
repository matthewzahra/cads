NOTE: the library lib/ox/cads/... is the compiled source coude - it does NOT include the original code

# To run files
Easiest way:
scala-cli run <list .scala files> --classpath <localtion of "lib/"> -- <any flags>

Second option:
scala -classpath lib <file_location>
(Note: can use scalac instead if we want to compile instead)


NOTE: lib2 and lib3 respectively hold the compiled ox.cads library for scala versions 2 and 3
Scala version 2 is more stable...
