# atsuage

atsuage is a static site generator.

Work In Progress

## Overview

## Installation

This project depend on some other projects. Especially following projects

* [Roswell](https://github.com/roswell/roswell)
* [Rosa](https://github.com/t-sin/rosa)

related with how to install.

### How to install

use [roswell](https://github.com/roswell/roswell)

1. At first, install roswell [Installation](https://roswell.github.io/Installation.html)
2. install [Rosa](https://github.com/t-sin/rosa)
3. install atsuage

We can also install rosa by using roswell.

```
$ ros install t-sin/rosa
$ ros install myaosato/atsuage
```

Roswell and Quicklisp resolve other dependencies.

## Usage

```
$ atsuage help

atsuage 
  
  version 0.1.0

  simple static site generator 

  new-prpject [name] : make new project
  new [name] : make new text
  new [name] [format] : make new text using specified format
  page [name] : make page
  page [name] [template] : make page using specified template
  all : make pages 
  dir : show current project directry
  texts : show text list
  conf : show config
  help : show help message

```
