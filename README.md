# Scala-based analyzer framework for shimple

## Disclaimer

This is an early work in progress.  There are a lot of rough edges and
bugs.  The interface is bare bones as most of the current work is on
the core analyzer.

## Requirements

SBT (http://www.scala-sbt.org/)

Scala (http://www.scala-lang.org/)

A Java 1.7 installation.  (Java 1.8 is unsupported.)

## Initialization

### Setting JAVA_HOME to point to Java 1.7

The command

    java -version

will display which version of Java is pointed to by JAVA_HOME.  For example:

    java version "1.7.0_79"
    Java(TM) SE Runtime Environment (build 1.7.0_79-b15)
    Java HotSpot(TM) 64-Bit Server VM (build 24.79-b02, mixed mode)

If the Java version is not 1.7, then go download 1.7.  If your computer has both Java 1.7 and Java 8, it is necessary to change JAVA_HOME to point to 1.7 while running analyzer-related commands.

To find the value of JAVA_HOME for 1.7:

* On OS X, run the command

    /usr/libexec/java_home -v 1.7

which may return, for example

    /Library/Java/JavaVirtualMachines/jdk1.7.0_79.jdk/Contents/Home

* On Linux, the path might look like

    /usr/lib/jvm/java-7-oracle

or

    /usr/lib/jvm/java-7-openjdk-amd64

Then, to set the JAVA_HOME to 1.7, run this command in the terminal:

    export JAVA_HOME=<the JAVA_HOME path described above>

You will need to run this command whenever you start a new terminal session.


### Running the classGrabber script

Before running the analyzer for the first time, you must run the classGrabber script, ensuring that only Java 1.7 is visible.

If your JAVA_HOME is already set to point to Java 1.7, you can just run classGrabber directly:

    ./classGrabber.sh

This will populate `javacache/` with class files from your Java
installation.  For copyright reasons, we cannot distribute these with
the code.

Note that if you get an error like the following is is likely
that `./classGrabber.sh` pulled the class files from a Java 1.8
installation instead of Java 1.7.

    [error] (run-main-0) java.lang.RuntimeException: Assertion failed.

If you see this error, delete the contents of `javacache/` and rerun `./classGrabber.sh`, ensuring that you are running classGrabber with only Java 1.7 visible (see the discussion of JAVA_HOME above).


## Usage

### Default Mode

It is important to make sure that the sbt command below is run with JAVA_HOME set to Java 1.7 (see discussion above related to JAVA_HOME).

Simply run:

    sbt 'run -d <class-directory> -c <class> -m <main>'

 - `<class-directory>` is the directory containing the class files to
   analyze.  For the examples included with the source, this should be
   `to-analyze`.

 - `<class>` is the name of the class containing the `main` function
   from which to start the analysis.

 - `<main>` is the name of the `main` function from which to start the
   analysis.

For example, you could run:

    sbt 'run -d to-analyze -c Factorial -m main'

The first time you run this `sbt` will download a number of packages
on which our tool depends.  This may take a while, but these are
cached and will not need to be downloaded on successive runs.

After a while, this will launch a GUI showing the state graph and will
print out graph data to stdout.

To exit the program press Ctrl-C at the terminal.  (Closing the GUI
window is not enough.)

You may get an out of memory error--if so, you can run sbt with extra heap memory.  For example,

    JAVA_OPTS="-Xmx8g" sbt 'run -d to-analyze -c Factorial -m main'

You can change '8g' to whatever amount of memory you need.  You can also add other Java options for controlling stack size, etc.

You can try this with most of the class files in `to-analyze/`, but some
of them trigger bugs that we have yet to fix.  The following are known
to work:

    Arrays, BoolTest, Casting, CFGTest, Exceptions, Factorial, Fib, Goto,
    InnerClassCasting, Objects, ObjectsNewStmt, Statics, SwitchTest

You may occasionally see exceptions at the terminal that are coming
from the Java GUI system (i.e. AWT or Swing).  These are harmless and
can safely be ignored.

### CFG Mode

Run:
    
    sbt 'run --cfg -d <class-directory> -c <class>'

 - `<class-directory>` is the directory containing the class files to
   analyze.  For the examples included with the source, this should be
   `to-analyze`.

 - `<class>` is the name of the class containing the `main` function
   from which to start the analysis.

The call graph in JSON format will be dump to `<class-directory>`.

For example, you can run:

    sbt 'run --cfg -d to-cfg -c Factorial'

The JSON output is an array, and each object in the array represent a statement
int the program with an unqiue `id`, the object also has following items:

 - `method` is the signature of method.

 - `inst` is the statement.

 - `targets` is an array contains the target statements. If the target is a function,
    it points to the first statement of callee. If the target is the caller, then it
    will point back to the statement which invokes to here.

 - `succ` is an array contains the successor statements of current statement.
