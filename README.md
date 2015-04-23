# What is this?

*quaazar* is a scene and 3D engine initially designed **for demoscene purposes**.
However, *quaazar* will be released as a *BSD3* Haskell package at some day.

# Brief

*quaazar* is a work-in-progress Haskell package. It uses **OpenGL** behing the
scene, but will also use **Vulkan** as it’s released. It’s a continuous effort
to be modern and up-to-date. That’s why the initial version of **quaazar** will
be backed-up by **OpenGL 4.4** at least – **OpenGL 4.5** if drivers get shit done
correctly.

# How to use it?

Up to now, I strongly don’t recommend to use **quaazar** yet. It’s not ready for
you folks. I’ll write a few articles about it on
[my blog](http://phaazon.blogspot.fr/) when it’s ready to
[be shipped](http://phaazon.net/pub/ship_it.jpg).

However, if you’re tedious and want to get your feet (very) wet, you can clone the
git repository, `cabal install --only-dependencies` and `cabal install`. It will
install the library and a tool called `qzr`. Consider using `qzr init [path]` to
start a new **quaazar** project in the current directory. It’ll create a default
application with a black window.

The documentation is not on hackage yet since it’s not released, so consider
building it when installing **quaazar**. It’s not complete, but it’s not empty
either.

# About contributions…

Yeah, you might find **quaazar** pretty cool. If you don’t, tell me in the face.
Ahah just kidding, but tell me why you think it sucks in the
[issues tracker](https://github.com/phaazon/quaazar/issues). That’s important to
me in order to enhance **quaazar** and make everyone happy.

**quaazar** is actually my software baby. In the end, I want everyone to be able
to use it. That doesn’t mean I want to give it away. As my property, it’s also my
toy, my decisions, my way to picture rendering. Then, I don’t commonly accept pull
requests.

**However**, if you push a PR that fixes a critical issue, I’ll accept it for sure.
I’m also open-minded to meta discussions about it. If **quaazar** gets used by
a lot of people at some times, I want everyone to be happy with it. Don’t blame
[BDFL](http://en.wikipedia.org/wiki/Benevolent_dictator_for_life).

# Versions

As new versions get released, backward compatibility and legacy code will follow
the Haskell versioning guidelines, that are:

  - a patch version number gets incremented when **quaazar** gets something fixed
    internally that doesn’t add anything nor break anything ; the changes have no
    impact on the external part of the library; although performances fixes are
    considered patches as they don’t change the interface of the library, if
    performances get greatly improved or reduced, a major or minor version will be
    released instead ;
  - a minor version number gets incremented when **quaazar** gets changes that
    don’t break backward compatibility, like adding a new super hot feature ;
  - a major version number gets incremented when **quaazar** has backward
    compatibility breaking changes ;
  - a super-major version number gets incremented when **quaazar**’s design
    changes a lot, breaking everything and marking a new era (god that sounds so
    heroic).

The initial version is **0.1**.

# Screenshots

**quaazar** is shipped with default materials, like an enhanced version of the
phong lighting model:

![](http://phaazon.net/pub/quaazar_gloss_mute_linear.png)

It has a nice support of *compositing*, which can be used to implement nice
post-process effects:

![](http://phaazon.net/pub/blue_ambient_white_omni_rgba_distortion.png)

**quaazar** supports an arbitrary number of lights per scene and especially
allows the programmer to use as many shadows as they wish. Furthermore, the
level of detail of each shadow can be changed dynamically:

![](http://phaazon.net/pub/shadows_everywhere.png)

