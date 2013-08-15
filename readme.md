ICFP Programming Contest 2013 - Team "きらきら☆あんなたん"
=====================================

The team **きらきら☆あんなたん**'s entry for https://research.microsoft.com/en-us/events/icfpcontest2013/


Team members
----------

* Anna Ogawa
* Keiko Oono
* Hayato Ito
* Mai Nishimura (As s chef)

Used languages
----------

- Scala

Project
----------

- This is a standard sbt (http://www.scala-sbt.org/) project.
- See http://www.scala-sbt.org/release/docs/Getting-Started/Directories.html for the directory structure.
- To run the program, you have to add a text file, `src/main/resources/icfpc2013/kirakira/token.txt`, that should contain your team's auth token.

How to run (for Mac)
----------

    % brew update && brew install sbt
    % export SBT_OPTS="-XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=256M"
    % sbt run
    ....

How to develop using Eclipse (if you love Eclipse)
----------

- Install Scala IDE from http://scala-ide.org/download/current.html for Scala 2.10.x.
- Generate project files for Eclipse:

  % sbt eclipse

  Make sure that `.project`, `.classpath` and `.settings/` are generated, which are used by Eclipse.
- Import the workspace (this top directory) from Eclipse.
