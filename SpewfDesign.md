# What about other frameworks? #

[ErlyWeb](http://erlyweb.org/) is a really good web framework for Erlang, and follows the nice Model-View-Controller separation of Rails and Rails-inspired frameworks. I understand there will soon be an Mnesia driver for it (from !ErlyDB).

My point was not to write another framework that duplicates the efforts of more mature ones, but to provide something with some more power than "default Erlang", in an Erlang-y way; that is, without particularly following the model of other frameworks (such as code generation or embedding code in templates).

# Concepts #

## Sessions are Processes ##

The "traditional" way to keep sessions are in some kind of memory object in the language runtime system (e.g. continuations in Arc, PLT Scheme, Seaside, beans in J2EE application servers) or in a session server of some kind (a database, memcached, perhaps shared).

The disadvantages of the former are that an application scales only to a single computer. Once you have enough traffic that that becomes problematic, you now face a new problem, which means you need to start doing sessions in an entirely different way.

The second method is only a little better. It's better because the resource that is serving session information can be dedicated to that task. But it is still a potential bottleneck, either as a single point of failure or as a limiting resource.

Erlang processes as sessions have a number of advantages:

  * Scalability. A distributed Erlang system can have any number of nodes in it, each with thousands of processes executing user sessions. Since the SPEWF session id is just a node and local id, no central dictionary or store of session data is required--it just sends the request to the right process and sends the result back to the client (another way of saying this is that the only synchronous step in serving a page is in the application logic itself).
  * Elegance. A web application never needs to worry about statefulness or concurrency. It can be written in the same way as an event-driven desktop application. Any method the programmer might want to use to preserve state (a looping function, a table, a database, whatever) can be used in a straightforward manner just as with any Erlang application (SPEWF provides a new OTP behavior that's similar to, but simpler than, gen\_server).
  * Robustness. Since Erlang has online code upgrades, the user session can remain alive even as the application code it's executing is upgraded (this is also really convenient for development).

## Callbacks ##

Erlang OTP design principles give us the notion of modules implementing standard behaviors by implementing callback functions. SPEWF applications are written just by implementing a callback module matching the `spewf_session` behavior. Two such applications are included:

  * The `webecho` module is the simplest such module. It's collected right with the SPEWF modules themselves.
  * The `board` application (status: pre-functional) is a basic web posting board implemented using Mnesia. It is its own standalone OTP-compliant application.

## Erlang is Cool ##

Erlang is a good programming language with a robust and richly-featured runtime system. Interoperability (with database or other backends) is a general application development issue and not the responsibility of the web framework. The assumption behind SPEWF (if there is one) is that you will store your data in Mnesia. To that end, it might gain features like using Mnesia record shapes for communicating with applications, but no tighter coupling would be contemplated.

Parse transforms are neat, and if there's a way to write the web application more concisely using transformations, that would be good. However, in my opinion code generation is a no-no.

Similarly, I'm not fond of template-based frameworks. HTML is something you should generate, not write (I'm with Seaside et al. on this one). Smart "templates" should take the form of an Erlang term structure, not text files.

# Future Ideas #

The `spewf_lang` processor could be a lot more powerful. Right now it does little more than add the hooks in the HTML to preserve session state. It is already extensible: the user can specify options in the `spewf` term structure for processing functions to transform the tree after `spewf_lang` normalizes it and before it processes it.

Cookie sessions should be supported.

Some better management features regarding shaping the grid would be nice.