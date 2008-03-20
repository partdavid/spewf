=====
board
=====

This is a testing/prototype/toy application for SPEWF, in much the same
way as Paul Graham's news application serves as a testing ground for
Arc_. The application itself is greatly unfinished, though several
basic functions work in conjunction with SPEWF.

_Arc: http://arclanguage.org/

Getting Started
===============

You will first need to initialize an mnesia directory in ``priv/data``::

   erl -sname board -mnesia dir '"priv/data"'
   1> mnesia:create_schema().

Then you can get started with the example data by running the test startup
script in this directory (run ``./test.sh``)::

   ./test.sh
   1> bd_db:create_tables().
   ...
   1> bd_db:load_example_data("example/test.txt").

Lots of basic web forum functionality is missing, but you can view posts
and threads, log in and make replies and new threads.

Future Directions
=================

There is a lot of HTML-fiddling and Db-fiddling in the board application.
The idea is to factor out a lot of this stuff and reduce the size of it
(it's around 1000 lines, it seems like it should be much less). The
spewf reply "language" and the potential transformations that could be
done to it (a la XSLT) could make things a lot terser. Having better
basic database operations (such as always returning user records when
the database stores references) could help a lot.

