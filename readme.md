ICFP Programming Contest 2013 - Team "kirakira annatan"
=====================================

The "kirakira annatan" team's entry for https://research.microsoft.com/en-us/events/icfpcontest2013/


Team members
----------

* Anna Ogawa
* Keiko Oono
* Hayato Ito


Project
----------

- This is a standard sbt (http://www.scala-sbt.org/) project.
- See http://www.scala-sbt.org/release/docs/Getting-Started/Directories.html for the directory structure.
  - `src/main/java/` is for Java sources.
  - `src/main/scala/` is for Scala sources.


How to run (for Mac)
----------

    % brew update && brew install sbt
    % export SBT_OPTS="-XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=256M"
    % sbt
    > run
    "Hello World!"
    > run-main icfpc2013.kirakira.HelloJava
    "Hello World! Java!"
    > test
    ...
    [success]


How to develop using Eclipse (if you love Eclipse)
----------

- Install Scala IDE from http://scala-ide.org/download/current.html for Scala 2.10.x.
- Generate project files for Eclipse:

  % sbt eclipse

  Make sure that `.project`, `.classpath` and `.settings/` are generataed, which are used by Eclipse.
- Import the workspace (this top directory) from Eclipse.
