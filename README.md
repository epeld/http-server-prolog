# Hello World

This is a project to learn more about the machinisms of a HTTP server.

It implements a HTTP server from the library functions in SWI-Prolog. Yes,
SWI-Prolog already has a working HTTP server implementation, but I choose to ignore that.

## Starting the Server

Load *main.pl* and call *main/0*. This should start the server on *localhost:8080*.

Now try curling:

```
        curl localhost:8080 -i
```
and watch the server crash