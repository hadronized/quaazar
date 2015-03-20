# quaazar

*quaazar* is a scene and 3D engine written by phaazon **for demoscene purposes**.
However, *quaazar* will be released as a BSD3 Haskell package at some day.

# Brief

*quaazar* implements the idea of *deferred effects*. When the user asks *quaazar*
to render a mesh, the mesh is not rendered directly. It’s cached for later use.
That enables interesting abstractions to show up, such as
[Semigroup](https://hackage.haskell.org/package/semigroups-0.16.2.2/docs/Data-Semigroup.html#t:Semigroup)
and [Monoid](http://hackage.haskell.org/package/base-4.7.0.2/docs/Data-Monoid.html#t:Monoid).

*quaazar* uses **OpenGL**, but will also use **Vulkan** as it’s released.
