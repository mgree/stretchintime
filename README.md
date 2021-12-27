A timer specially built for repetitive, timed routines---like stretching. Specify a list of stretches and their variations concisely, then hit play and you'll run through each of them.

# Syntax

Routines can be given in two languages: a high-level specification called the _program format_ and a lower-level, explicit specification called the _plan format_.

At its core, a routine is just a list of things to do: a stretch (or some other activity with som properties), a pause to rest or get ready for the next stretch, or an announcement of some kind (e.g., what the next stretch is). A _plan_ is just a list of these _entries_. Each _entry_ is either a named thing with some properties to be done for some duration, a pause for some duration, or an announcement.

An _expression_ in the program format compiles down to a _plan_, which is a series of _entries_. 
The plan format is a subset of the program format.

## Program format

The line-oriented, indentation-sensitive _program format_ offers concise specifications: you can "scope" a variation over a large set of entries.

```
keywords:
  pause, vary, in, repeat, shuffle, intersperse, before, after, and, between

BLOCK    ::= vary NAME: VALUE(, VALUE)*
               BLOCK
           | repeat N
               BLOCK
           | ENTRY
             [BLOCK]
           | SHUFFLE
               BLOCK
           | intersperse 
               BLOCK1
             [in|before and in|after and in|before and after and in|around] 
               BLOCK2

ENTRY    ::= NAME DURATION (NAME: VALUE)*
               [NAME: VALUE(, VALUE)*]
               [repeat N]
           | pause DURATION
           | announce MESSAGE

NAME     ::= [^0-9]*
VALUE    ::= [^,]*
DURATION ::= [[H:]M:]S
MESSAGE  ::= [^\n]*
```

## Plan format

The compiled _plan format_ is just a list of entries.

```
PLAN  ::= ENTRY 
        | ENTRY
          PLAN

ENTRY ::= NAME DURATION (NAME: VALUE)*
        | pause DURATION
        | announce MESSAGE
        
NAME     ::= [^0-9]*
VALUE    ::= [^,]*
DURATION ::= [[H:]M:]S
MESSAGE  ::= [^\n]*
```
