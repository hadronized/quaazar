# quaazar

*quaazar* is a scene and 3D engine written by phaazon **for demoscene purposes**.
However, *quaazar* will be released as a BSD3 Haskell package at some day.

# Brief

*quaazar* is a work-in-progress Haskell package.

*quaazar* uses **OpenGL**, but will also use **Vulkan** as it’s released.

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

