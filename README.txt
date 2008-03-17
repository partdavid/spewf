=====
SPEWF
=====

SPEWF is an OTP-compliant application for developing web applications through
implementation of a standard behavior. It models web sessions as processes
(one session per process) in a straightforward way. Its goals are to keep
the application logic simple and abstracted from the business of serving
web pages through a potentially large cluster of web and application servers.

Requirements
============

SPEWF uses **eunit** and **yaws**. They should be installed in standard
locations findable by the Erlang code loader.

Getting Started
===============

You can get started by using the ``test.config`` configuration file in
the ``lib/spewf`` subdirectory. In that directory, type ``make`` (GNU make
is required).

Once all the beam files are created in ``lib/spewf/ebin``, from ``lib/spewf``
you can do ``erl -pz ebin -config test -run crypto start -run spewf
start``. This will start the shell, the spewf application and yaws running
on localhost:5959. Only one spewf subapp is configured: **webecho**. Go
to ``http://localhost:5959/spewf/webecho`` and see what it does.

Installation
============

You can copy the built application into the system Erlang/OTP system using
the ``make install`` target, or you can use the OTP base build tools to create
a release in ``release`` subdirectory.

Documentation
=============

The application documentation can be built by using the ``docs`` make target
in the ``lib/spewf`` subdirectory. This will build HTML documentation
in ``lib/spewf/doc``. Pay special attention to the section on application
configuration and implementing a **spewf_session** callback module.

For additional information on SPEWF's design and its internals, see
the ``lib/spewf/design.txt`` file.

License
=======

SPEWF is free software: you can redistribute it and/or modify it under the
terms of the GNU Lesser General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option)
any later version.

SPEWF is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
more details.

You should have received a copy of the GNU Lesser General Public License
along with SPEWF.  If not, see `http://www.gnu.org/licenses/`.
