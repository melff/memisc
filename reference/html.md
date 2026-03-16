# Building Blocks for HTML Code

The functions described here form building blocks for the
[`format_html`](https://melff.github.io/memisc/reference/format_html.md)
methods functions for `codebook`, `ftable`, `ftable_matrix`, and
`mtable` objects, etc.

The most basic of these functions is `html`, which constructs an object
that represents a minimal piece of HTML code and is member of the class
`"html_elem"`. Unlike a character string containing HTML code, the
resulting code element can relatively easily modified using other
functions presented here. The actual code is created when the function
`as.character` is applied to these objects.

Longer sequences of HTML code can be prepared by concatenating them with
`c`, or by `html_group`, or by applying `as.html_group` to a list of
`"html_elem"` objects. All these result in objects of class
`"html_group"`.

Attributes (such as class, id etc.) of HTML elements can be added to the
call to `html`, but can also later recalled or modified with `attribs`
or `setAttribs`. An important attribute is the style attribute, which
can contain CSS styling. It can be recalled or modified with `style` or
`setStyle`. Styling strings can also be created with `hmtl_style` or
`as.css`

## Usage

``` r
html(tag, ..., .content = NULL, linebreak = FALSE)
html_group(...)
as.html_group(x)

content(x)
content(x) <- value
setContent(x,value)

attribs(x)
attribs(x) <- value
setAttribs(x,...)
# S3 method for class 'character'
setAttribs(x,...)
# S3 method for class 'html_elem'
setAttribs(x,...)
# S3 method for class 'html_group'
setAttribs(x,...)

css(...)
as.css(x)
style(x)
style(x) <- value
setStyle(x,...)
# S3 method for class 'character'
setStyle(x,...)
# S3 method for class 'html_elem'
setStyle(x,...)
# S3 method for class 'html_group'
setStyle(x,...)
```

## Arguments

- tag:

  a character string that determines the opening and closing tags of the
  HTML element. (The closing tag is relevant only if the element has a
  content.)

- ...:

  optional further arguments, named or not.

  For `html`: named arguments create the attributes of the HTML element,
  unnamed arguments define the content of the HTML element, i.e.
  whatever appears between opening and closing tags (e.g. `<p>` and
  `</p>`). Character strings, `"html_elem"`, or `"html_group"` objects
  can appear as content of a HTML element.

  For `setAttribs`: named arguments create the attributes of the HTML
  element, unnamed arguments are ignored.

  For `setStyle`: named arguments create the styling of the HTML
  element, unnamed arguments are ignored.

  For `html_group`: several objects of class `"html_elem"` or
  `"html_group"`.

  For `css`: named arguments (character strings!) become components of a
  styling in CSS format.

- .content:

  an optional character string, `"html_elem"`, or `"html_group"` object

- linebreak:

  a logical value or vector of length 2, determines whether linebreaks
  are inserted after the HTML tags.

- x:

  an object. For `as.html_group`, this should be a list of objects of
  class `"html_elem"` or `"html_group"`. For `content`, `setContent`,
  `attribs`, `setAttribs`, `style`, `setStyle`, this should be an object
  of class `"html_elem"` or `"html_group"`.

- value:

  an object of appropriate class.

  For `content<-`: a character string, `"html_elem"`, or `"html_group"`
  object, or a concatenation thereof.

  For `attribs<-` or `style<-`: a named character vector.

## Details

Objects created with `html` are lists with class attribute `"html_elem"`
and components

- tag:

  a character string

- attributes:

  a named character vector

- content:

  a character vector, an `"html_elem"` or `"html_group"` object, or a
  list of such.

- linebreak:

  a logical value or vector of length 2.

Objects created with `html_group` or by concatenation of `"html_elem"`
or `"html_group"` object are lists of such objects, with class attribute
`"html_group"`.

## Examples

``` r
html("img")
#> <img> 
html("img",src="test.png")
#> <img src="test.png"> 
html("div",class="element",id="first","Sisyphus")
#> <div class="element" id="first">Sisyphus</div> 
html("div",class="element",id="first",.content="Sisyphus")
#> <div class="element" id="first">Sisyphus</div> 

div <- html("div",class="element",id="first",linebreak=c(TRUE,TRUE))
content(div) <- "Sisyphus"
div
#> <div class="element" id="first">
#> Sisyphus</div>
#>  

tag <- html("tag",linebreak=TRUE)
attribs(tag)["class"] <- "something"
attribs(tag)["class"]
#> $class
#> [1] "something"
#> 
tag
#> <tag class="something">
#>  

style(tag) <- c(color="#342334")
style(tag)
#> color: #342334; 
tag
#> <tag class="something" style="color: #342334;">
#>  

style(tag)["bg"] <- "white"
tag
#> <tag class="something" style="color: #342334; bg: white;">
#>  

setStyle(tag,bg="black")
#> <tag class="something" style="color: #342334; bg: black;">
#>  
setStyle(tag,c(bg="black"))
#> <tag class="something" style="color: #342334; bg: black;">
#>  

c(div,tag,tag)
#> <div class="element" id="first">
#> Sisyphus</div>
#> <tag class="something" style="color: #342334; bg: white;">
#> <tag class="something" style="color: #342334; bg: white;">

c(
  c(div,tag),
  c(div,tag,tag)
)
#> <div class="element" id="first">
#> Sisyphus</div>
#> <tag class="something" style="color: #342334; bg: white;">
#> <div class="element" id="first">
#> Sisyphus</div>
#> <tag class="something" style="color: #342334; bg: white;">
#> <tag class="something" style="color: #342334; bg: white;">

c(
  c(div,tag),
  div,tag,tag
)
#> <div class="element" id="first">
#> Sisyphus</div>
#> <tag class="something" style="color: #342334; bg: white;">
#> <div class="element" id="first">
#> Sisyphus</div>
#> <tag class="something" style="color: #342334; bg: white;">
#> <tag class="something" style="color: #342334; bg: white;">

c(
  div,tag,
  c(div,tag,tag)
)
#> <div class="element" id="first">
#> Sisyphus</div>
#> <tag class="something" style="color: #342334; bg: white;">
#> <div class="element" id="first">
#> Sisyphus</div>
#> <tag class="something" style="color: #342334; bg: white;">
#> <tag class="something" style="color: #342334; bg: white;">

content(div) <- c(tag,tag,tag)
div
#> <div class="element" id="first">
#> <tag class="something" style="color: #342334; bg: white;">
#> <tag class="something" style="color: #342334; bg: white;">
#> <tag class="something" style="color: #342334; bg: white;">
#> </div>
#>  


css("background-color"="black",
                  color="white")
#> background-color: black; color: white; 

as.css(c("background-color"="black",
                  color="white"))
#> background-color: black; color: white; 



Hello <- "Hello World!"
Hello <- html("p",Hello,linebreak=c(TRUE,TRUE))
style(Hello) <- c(color="white",
                  "font-size"="40px",
                  "text-align"="center")
     
Link <- html("a","More examples here ...",
             href="http://elff.eu/software/memisc",
             title="More examples here ...",
             style=css(color="white"),
             linebreak=c(TRUE,FALSE))
Link <- html("p"," (",Link,")",linebreak=c(TRUE,TRUE))
style(Link) <- c(color="white",
                 "font-size"="15px",
                 "text-align"="center")

Hello <- html("div",c(Hello,Link),linebreak=c(TRUE,TRUE))
style(Hello) <- c("background-color"="#160666",
                  padding="20px")
Hello
#> <div style="background-color: #160666; padding: 20px;">
#> <p style="color: white; font-size: 40px; text-align: center;">
#> Hello World!</p>
#> <p style="color: white; font-size: 15px; text-align: center;">
#>  (<a href="http://elff.eu/software/memisc" title="More examples here ..." style="color: white;">
#> More examples here ...</a>)</p>
#> </div>
#>  

show_html(Hello)
#> <div style="background-color: #160666; padding: 20px;">
#> <p style="color: white; font-size: 40px; text-align: center;">
#> Hello World!</p>
#> <p style="color: white; font-size: 15px; text-align: center;">
#>  (<a href="http://elff.eu/software/memisc" title="More examples here ..." style="color: white;">
#> More examples here ...</a>)</p>
#> </div>
```
