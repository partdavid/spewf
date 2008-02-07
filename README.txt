======
README
======

Using SPEWF
===========

See ``http://yaws.hyber.org/appmods.yaws`` for information on installing and
using appmods. In general, you should use a ``pathelem`` of ``spewf`` when
configuring the appmod.

When spewf handles the request, it takes the next path component as your
callback module that implements your web application. For example, if the
URL is ``http://example.com/spewf/myapp`` then SPEWF invokes the **myapp**
module.

Your callback module must implement and export one function: ``handle/2``.
The first argument is a state, which can take any form (though the first time
SPEWF invokes the callback it will pass an empty list for this argument).

The second argument is a keylist reflecting the request (functions such as
``lists:keysearch/3`` are suitable for operating on the keylist. The
parameters (keys) of the keylist are atoms reflecting the form values or query
string values passed in.

``handle/2`` is expected to return a two-element tuple. The first element is
the new state. The second element is the reply. This corresponds to one of the
return types you can return from ``out/1`` in a ``.yaws`` page or normal
appmod, with the ``'self'`` exception described below.

The application is responsible for including a placemarker called ``self``
in order that SPEWF can generate the appropriate session-preserving URL.
The atom ``self`` should be substituted for any self-referential URLs (for
example, in the ``href`` attribute of ``a`` links or the ``action`` attribute
of ``form`` entities; when using the ``ehtml`` return type.

The callback module is invoked in its own session-specific process, which
lives until an inactivity timeout (currently hardcoded at 15 minutes). When
a user with a timed-out session visits the URL again, the application starts
anew (a new session is created).

See the example web application *said* for an example of using SPEWF.

Status
======

This describes version 0.1.0, which is just a sketch of a few ideas. See
TODO, below, for ideas for future directions.

TODO
====

   * Cookie-based sessions
   * Remote session spawning: it should be possible to spawn session processes
     on remote nods and use node-qualified Pids in the session identifier
   * Need a more elegant way to transform the "output" of the callback module
     to include session-preserving targets (if any)
      * This might imply a much richer reply type that ``ehtml``, which could
        also be used for easy interactivity. Is it possible to write user
        interactivity in a script-like manner? Is this desirable in Erlang?
   * Configurable timeouts (obviously)

Discussion
==========

Even in its very basic form, the potential is there for all kinds of goodies
as a web framework, especially in terms of scaling (distribution of sessions
across multiple nodes; along with distribution of yaws instances).

Might it be possible to use a "data model"-derived model of interactivity (a
la Rails, etc.)? For example, maybe the callback module always defines a
record and that record is actually used instead of a keylist to communicate?
