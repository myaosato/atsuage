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

### Simple example

For example, you want to maek new project (directory for atsuage and your website) on your home directory (```/home/username/```).

```
$ cd ~
$ atsuage new-project mywebsite
```

The above command  make directory (~/mywebsite/) and its child directries and child files.

```
mywebsite/
|--.atsuage
|--texts/
|  |--project
|  |--index
|--pages/
```

```
$ cd ~/new-project
$ atsuage dir
/home/username/mywebsite/
```

Command ```atsuage dir``` show current project dir.

```
$ atsuage dir
/home/username/mywebsite/
$ atsuage all
```

Command ```atsuage all``` convert all text files without ignored files to html files (under ```pages/``` dir)

In this case, after execute ```atsuage all``` command ```pages/index.html``` file is created. ```texts/project``` file is ignored.

You can setting ignored files like ```texts/project``` file. Please check .atsuage file

```
$ less /home/username/mywebsite/.atsuage
(:IGNORE ("project")
 :TEXT-FORMAT
 (:DEFAULT
     (:TITLE "title" :DATE "" :UP "" :PREV "" :NEXT "" :TEXT "please write
...
...
")
   :SAMPLE
   (:TITLE "title" :DATE "" :UP "" :PREV "" :NEXT "" :TEXT "please write
sample
...
")))
```

If you don't want to convert  ```hoge``` file. Please add "hoge" to :ignore porperty as following.

```
$ less /home/username/mywebsite/.atsuage
(:IGNORE ("project" "hoge")
 :TEXT-FORMAT
 (:DEFAULT
     (:TITLE "title" :DATE "" :UP "" :PREV "" :NEXT "" :TEXT "please write
...
...
")
   :SAMPLE
   (:TITLE "title" :DATE "" :UP "" :PREV "" :NEXT "" :TEXT "please write
sample
...
")))
```

Such files are used having information of all of your project (For example, site name, author copyright, style...)

Command ```atsuage all``` "all" texts file convert using "default template" ```templates/template```. If you want to convert a text file(for exmple, ```texts/index```), please use ```atsuage page```.

```
$ atsuage page index
```

If you want to specify used template file, please add a argument.

```
$ atsuage page index foo
```

and prepare ```template/foo```. About syntax, please check "Syntax of template file".

## Syntax of template files

### simple html tag

```
(:p "hoge" (:span "piyo"))

-> <p>hoge<span>piyo</span></p>
```
### attribute tag

```
(:a &(:href "hoge.html") "hoge")

-> <a href="hoge.html">hoge</a>
```

### empty-elements

```
(:br)
(:img &(:src "foo.png"))

-> <br /><img src="foo.png">
```

### information in texts file

texts/foo
```
:title foo
:body
### fuga

* bar
* baz

```

texts/project
```
:site-name hogera
```

when ```$ atsuage page foo```


```
(:h1 (:get-value "title"))
(:h2 (:get-value "site-name" "project"))
(:get-value-as-md "body")

->
<h1>foo</h1><h2>hogera<h2><h3>fuga</h3><ul><li>bar</li><li>baz</li></ul>
```



### Example template

```
(:html (:head
        (:title "title"))
       (:body
        (:h1 (get-value "site-name" "project"))
        (:h2 (get-value "title"))
        (:p "hogehoge"
            (:a &(:href "./fuga.html") (get-value "title" "fuga"))
            "piyopiyo")
        (:main (get-value-as-md "text"))))
```

## Syntax of teｘｔ files
```
:TITLE sample page
:DATE 2018-01-21
:UP foo
:PREV bar
:NEXT baz
:TEXT
### Hello!!

* foo
* bar
* baz

```

Please check [Rosa Syntax](https://github.com/t-sin/rosa#syntax).

In atsuage, labels are case insensitive.

## help message

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

## What dose "atsuage" mean?

It is one of tofu foods.
[Aburaage - Wikipedia](https://en.wikipedia.org/wiki/Aburaage)

## Licence

This software is released under the MIT License, please check LICENSE.txt.
