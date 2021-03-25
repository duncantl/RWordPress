# RWP (RWordPress fork)

Note: This is a fork of [Duncan Temple Lang's RWordPress package](https://github.com/duncantl/RWordPress) for R. The upstream package has been unmaintained for several years, and we needed to add some specific functionality. Please don't consider this a fully supported version of RWordPress. :)

The name `RWordPress` is required in the `knit2wp` wrapper included with [knitr](https://github.com/yihui/knitr), so it remains the same in this fork.

## Installation

Install the required packages:

```
install.packages("knitr")
install.packages("devtools")
library("devtools")
devtools::install_github(c("josephguillaume/XMLRPC","happyprime/RWP"))
```

When loading the first and any new R session:

```
library("knitr")
library("RWordPress")
library("XMLRPC")
```

## Unique Functions

### RWPSetCredentials( url, login, password )

A somewhat more straightforward way of setting required credentials. This should ideally be run from the console and not part of the document.

```
RWPSetCredentials( "https://domain.test/xmlrpc.php", "myusername", "mypassword" )
```

### RWPSaveCurrent( file )

Save the specified file using its current header data. This should be run from the console and not part of the document.

```
RWPSaveCurrent( 'my-post.Rmd' )
```

This expects an accessible `my-post.Rmd` file with data similar to the following in its header:

```
---
title: "The post title"
author: "Author name"
date: "3/24/2021"
postid: FALSE
publish: FALSE
output: html_document
---
```

Once a post ID is available for a post, the `postid` header line can be changed from `FALSE` to that ID and the existing post will be updated when `RWPSaveCurrent( file )` is run again.

The `publish` value be either `FALSE` to leave the post as a draft, or `TRUE` to publish the post.

The `author`, `date`, and `output` headers do not affect `RWPSaveCurrent()`.

### RWPSaveManual( file, title, postid = 0, publish = FALSE )

Save a file with specific options that override the file's current header data. This should be run from the console and not part of the document.

```
RWPSaveManual( 'my-post.Rmd', 'My post title', 4002, TRUE )
```
