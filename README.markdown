The Arboretuum Programming Language
===================================

March 2008, Chris Pressey, Cat's Eye Technologies.

Description
-----------

Arboretuum is a language based on an experimental variant of
tree-rewriting which we call forest-rewriting. Appropriate to its name,
during forest-rewriting, multiple trees (specifically, a finite set) are
rewritten. Each tree is labelled with a name; a rewriting pattern can
refer to multiple trees, and must match all of them simultaneously in
order for a replacement to occur.

As an experiment, Arboretuum was not entirely a success.
Forest-rewriting unfortunately turned out to be insufficient for what I
wanted to apply it to, namely compiler specification. The idea was to
have each tree associated with some data structure used in the
compilation process (AST, symbol table, output buffer, etc.) However, it
became apparent that, by itself, forest-rewriting could not synchronize
the data across the trees the way it would need to be synchronized in a
real compiler. I plan to tackle the problem again, with a different
variation on rewriting, in a future project.

Regardless, Arboretuum is Turing-complete, as tree-rewriting is simply a
special case of forest-rewriting: just have one tree in the forest.

Implementation
--------------

I will refer you to the reference implementation of Arboretuum for
details on the semantics of the language. Ordinarily I frown upon this
sort of practice — normatively defining a language by an implementation
rather than by a specification — but the interests of brevity, the
experimental tack of the project, the unsuccessful outcome of the
experiment, and the relative well-definedness of the implementation
language (the purely functional subset of R5RS Scheme) conspire to
make the consequences of this choice less painful than usual.

The reference implementation comprises the following files:

-   `preprocess.scm`

    Pre-processes the input program into an internal format suitable for
    forest-rewriting.

-   `unify.scm`

    Implementation of the unification algorithm which is used to match
    the pattern part of rewriting rules to the forest.

-   `forest-rewrite.scm`

    Implements the forest-rewriting process proper.

-   `utils.scm`

    Miscellanous support procedures, including `mergesort`,
    `vector-store` (a side-effect-free alternative to `vector-set!`),
    `print` and `test`.

In addition, the following supplementary files which are not definitive
w.r.t. the Arboretuum language are included in the project:

-   `tests.scm`

    Gives a set of unit tests to confirm the absence of certain
    erroneous behaviours. (Obviously, no number of unit tests could
    confirm the absence of *errors*...)

-   `tree-rewrite.scm`

    Some basic tree-rewriting code, to provide contrast between it's
    complexity and that of forest rewriting.

Note that the Scheme implementation of algorithms in the above files are
to be taken as *pedantic* rather than *efficient*. They are meant to be
read (perhaps even enjoyed?) and only incidentally to be executed.

History
-------

This project was begun in January 2006. I'd been meaning to release it
for a while before actually doing so in March of 2008.

Happy forest-rewriting!

-Chris Pressey  
Cat's Eye Technologies  
March 4, 2008  
Chicago, Illinois, USA
