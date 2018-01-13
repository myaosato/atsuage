# Atsuage

Atsuageは、静的サイトジェネレータです。

Atsuageは、豆腐を揚げたもののことであり、私はそれが好きです。名前にそれ以上の意味はありません。

Tripitakaという類似のプロジェクトの後続プロジェクトです。Tripitakaを良いものにしようとする中で、いくつかのやろうとしていることの間には大きな違いがあることに気付き、なるべくパーツとレイヤに分けて管理しやすい新しいシステムを作ろうということに思い至り、新しいプロジェクトにすることにしました。

この文章は、開発途中のAtsuageについて、myaosatoが理解しつつ、コードを書いていくことが出来るようにまとめられています。

myaosato、つまり私は、仕様を変更する際には、ここに明記するように心がけましょう。

## Atsuageでやりたいこと

    SIMPLE STRUCTURED TEXT FILE + HTML LIKE TEMPLATE FILE = HTML FILE


## 各プロジェクトのディレクトリ構成

    project-dir
    |--.atsuage
    |--texts
    |  |--project
    |  |--hoge
    |  |--piyo
    |  |--...
    |--pages
    |  |--hoge.html
    |  |--piyo.html
    |  |--...
    |--templates
    |  |--template
    |  |--otherone
    |  |--...

## API Document?

## API MEMO

### FILES - files.lisp -

* set-project-dirs (dir)
* get-text-path (name)
* get-page-path (name)
* get-template-path (name)
* get-text-list
* get-template-list

### TEXT - text.lisp - 

* get-data-from-text (pathname &optional (upcase t))
* set-data-to-text (pathname data)

### DATA - data.lisp - 

* get-value (prop name &optional (ind 0))
* get-value-as-seq (prop name)
* set-value (prop name obj &optional (save? nil))
* add-value (prop name str &optional (save? nil))
* make-data (name)
* save-data (name)
* set-curret-name (name)
* get-curret-name ()

### CONVERTER - converter.lisp -

* convert (name template-sexp)
* read-template-form-file (template-path)

### CORE - core.lisp -

* initialize (dir)
* make-project (name dir)
* make-text (name &rest key-strs)
* make-page (name &optional (template-name "template"))

