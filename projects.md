---
title: Projects
mathjax: true
---

Over the course of my life I've made cool stuff. Here I've documented a mix of things I consider cool because people use them, and things I consider cool because they're technically interesting solutions to esoteric problems. I have a tendency to not finish my projects, especially beyond overcoming a significant challenge (the crux of the problem), or when the project turns into tedium.

* TOC
{:toc}

### Sandbox
<author>2015&ndash;2023</author>

A long time ago I was running an IRC[^IRC] bot that would let strangers on the internet run arbitrary pieces of code, for testing/demonstration/learning purposes. In order to protect the host of the bot from obvious vulnerabilities I made a [process sandbox](https://github.com/mniip/sandbox/) based on [ptrace](https://en.wikipedia.org/wiki/Ptrace), inside which the untrusted code would run. Originally employed by an IRC bot for the #lua and #haskell channels, it is now mostly used by a [Discord bot](https://github.com/mniip/discord-eval) for the [Functional Programming Discord server](https://discord.com/invite/K6XHBSh).

### finite-typelits
<author>2015-2023</author>

I maintain a somewhat widely-used Haskell [library](https://hackage.haskell.org/package/finite-typelits) for types with a finite number of elements specified at the type level using [GHC](https://www.haskell.org/ghc/)'s first-class type-level numbers.

### The Powder Toy
<author>2013&ndash;2015, 2018, 2023</author>

[The Powder Toy](https://powdertoy.co.uk/) is an oddly famous falling sand game, which I've helped develop. In the early years I made a lot of improvements to the Lua console, and in the latter years I've mostly helped refactor the game into more modern C++.

### Coq Classical Ensembles
<author>November 2021</author>

I've implemented a [library](https://github.com/mniip/coq-classical-ensembles) for the Coq proof assistant for manipulating classical (excluded middle) sets, represented type-theoretically as predicates on a given universe. The original intent was to use it to in turn make a library for general topology.

### CompaREST
<author>2021&ndash;2022</author>

Ever wonder what changed between two versions of an OpenAPI schema, and whether the old client would still work with the new server? As part of my work at [typeable.io](https://typeable.io/) I wrote a static analysis tool that does just that: it's called [CompaREST](https://github.com/typeable/compaREST), and it's free software!

### BOOTSTRA
<author>2018&ndash;2020</author>

Could you bootstrap a programming environment from a clean MS-DOS install having nothing but documentation? I sought to find out.

The first chapter of the story was to write an assembler that was better than the one available in `DEBUG.EXE`, in that it would support labels. And so I wrote [a complete 8086 assembler in MS-DOS batch](https://github.com/mniip/BOOTSTRA/tree/master/BATAS#batas), a language with virtually no string processing utilities. This was done by interpreting the input assembly as a batch file which would call subroutines for various 8086 instructions.

The next chapter was to write a compiler for a slightly higher level language using this assembler. To that end I implemented [a stack-based procedural language](https://github.com/mniip/BOOTSTRA/tree/master/STRAP#strap) with module support.

The next step would be to reproduce a system for separate compilation that could link "object" files into an `.EXE` file, but I got bored by then.

### De-Rham Curve Renderer

I made a [standalone program](https://gist.github.com/mniip/39c96123b24e771ec3087dd214106e78) that calculates a polyline approximation of arbitrary [iterated function systems](https://en.wikipedia.org/wiki/Iterated_function_system) --- a class of fractals, of which [de Rham curves](https://en.wikipedia.org/wiki/De_Rham_curve) are a common subset.


### Doger
<author>2014&ndash;2020</author>

I developed and hosted the IRC[^IRC] tipping bot for the cryptocurrency [Dogecoin](https://dogecoin.com/). It was called [Doger](https://github.com/mniip/Doger), and was listed as one of the official Dogecoin bots. In retrospect I can say that this is NOT how you should implement a monetary transaction system.

### kbdtop
<author>July 2019</author>

If you have an MSI per-key RGB keyboard, you can display htop-like system resource usage on it using a [little C program I wrote](https://gist.github.com/mniip/ffbd6cdeb2739c6f39bfefdbe9e6a89c).

### DLNG
<author>2017&ndash;2018</author>

In the modern world there's fewer and fewer reasons for a program to have hot code reloading, but back in the era of IRC[^IRC] bots that was a big deal. You could employ `dlopen` and friends to dynamically load shared objects as "plugins" in your program, but what about reloading the main loop? What about reloading the reloader? I've set out to write a replacement for the run-time dynamic linker (also known as `ld-linux.so`, the provider of `dlopen`, usually part of your `libc`) with a more fine-grained interface that could achieve exactly this kind of functionality. It was called "[Dynamic Linker NG](https://github.com/mniip/dlng)". I never got to the reloading part, but I did manage to successfully use it as a program interpreter for unsuspecting programs.

### OS
<author>2016</author>

Like everybody and their dog, I've tried my hand at making my own "OS". I ended up with an [MBR-bootable program](https://github.com/mniip/os) capable of running a Lua shell with a persistent filesystem, and for it a simple text editor written completely in Lua.

### Universal Machine Emulators
<author>2013, 2016</author>

I found out about the [2006 ICFP programming contest](http://www.boundvariable.org/) way later than it actually happened, but I was enamoured with the idea of it. You had to write an emulator for a made up processor architecture, and then run an unknown program on it --- which turned out to be a full Unix-like operating system. In the pursuit of efficiency I ended up writing a [JIT binary translator](https://github.com/mniip/um32) for this ISA.

### Dequantify
<author>October 2016</author>

There's an algorithm that can remove $\forall$ and $\exists$ quantifiers from statements involving polynomial arithmetic, for example, the statement $\exists x : x^2 + px + q = 0$ is equivalent to the statement $p^2-4q \ge 0$, which in turn has no quantifiers! This algorithm is a consequence of the [Tarski&ndash;Seidenberg theorem](https://en.wikipedia.org/wiki/Tarski%E2%80%93Seidenberg_theorem), and it is impractically slow, but I have a [Haskell implementation of it](https://github.com/mniip/dequantify) which I've used as credit in a formal systems class.

### Lua Scripting Support for HexChat
<author>2015-2016</author>

I implemented [Lua scripting support](https://hexchat.readthedocs.io/en/latest/script_lua.html) in [HexChat](https://hexchat.github.io/), a popular IRC[^IRC] client, and it has since become the preferred way to write scripts for it. Originally a standalone library, it has since been merged into the HexChat source tree.


### functional-kmp
<author>August&ndash;November 2015</author>

The [Knuth&ndash;Morris&ndash;Pratt](https://en.wikipedia.org/wiki/Knuth%E2%80%93Morris%E2%80%93Pratt_algorithm) substring search algorithm is normally implemented using arrays, and relies critically on $O(1)$ access that the array provides. However arrays aren't "first-class citizens" in pure functional languages, as opposed to algebraic datatypes. It turns out it's possible to re-engineer the algorithm to use cons-cell-based lists instead. A Haskell implementation of the resulting algorithm is available as a [library on Hackage](https://hackage.haskell.org/package/functional-kmp).


### Floppy Drive Midi Player
<author>February 2014</author>

Ever seen one of those videos where music is played on old floppy drives? A lot of them directly hook up wires to the stepper motors, but it turns out you can tell the Linux kernel and in turn the IDE bus to directly control the head of a floppy drive connected in the "normal" way. [FDMP](https://github.com/mniip/fdmp) is a prototype of a program that can play MIDI files on a floppy this way.

### xsTPTIRC
<author>2012&ndash;2013</author>

[xsTPTIRC](https://github.com/mniip/xsTPTIRC) was an in-game IRC[^IRC] client for The Powder Toy. I was 14 when I made it, and it was one of the first things I wrote that other people actually used.

[^IRC]: [Internet Relay Chat](https://en.wikipedia.org/wiki/Internet_Relay_Chat)
