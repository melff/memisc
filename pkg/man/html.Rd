\name{html}
\alias{html}
\alias{print.html_elem}
\alias{as.character.html_elem}
\alias{c.html_elem}
\alias{html_group}
\alias{as.html_group}
\alias{as.character.html_group}
\alias{print.html_group}
\alias{[.html_group}
\alias{[<-.html_group}
\alias{c.html_group}

\alias{content}
\alias{content<-}
\alias{setContent}
\alias{attribs}
\alias{attribs<-}
\alias{setAttribs}
\alias{setAttribs.character}
\alias{setAttribs.html_elem}
\alias{setAttribs.html_group}
\alias{[<-.html_attributes}

\alias{css}
\alias{as.css}
\alias{style}
\alias{style<-}
\alias{setStyle}
\alias{setStyle.character}
\alias{setStyle.html_elem}
\alias{setStyle.html_group}
\alias{as.character.css}
\alias{print.css}
\alias{[<-.css}
\title{
Building Blocks for HTML Code
}
\description{
The functions described here form building blocks for
the \code{\link{format_html}} methods functions for \code{codebook},
\code{ftable}, \code{ftable_matrix}, and \code{mtable} objects, etc.

The most basic of these functions is \code{html} which constructs
an object that represents a minimal piece of HTML code, an object
that is member of class \code{"html_elem"}. Yet unlike a 
character string containing HTML code, the resulting code element can 
relatively easily modified using the other functions presented here.
The actual code is created, once \code{as.character} is applied to
objects representing HTML code.

Larger sequences of HTML code can be prepared just by 
concatenating them with \code{c}, or by \code{html_group},
or by applying \code{as.html_group} to a list of 
\code{"html_elem"} objects. All these result in objects
of class \code{"html_group"}.

Attributes (such as class, id etc.) of HTML elements can be added to
the call to \code{html}, but can also later recalled or modified with 
\code{attribs} or \code{setAttribs}. 
A potentially important attribute is of course the style
attribute, which can contain CSS styling. It can be recalled or modified with \code{style} or \code{setStyle}. Styling strings can also 
be created with \code{hmtl_style} or \code{as.css}

}
\usage{
html(tag, \dots, .content = NULL, linebreak = FALSE)
html_group(\dots)
as.html_group(x)

content(x)
content(x)<-value
setContent(x,value)

attribs(x)
attribs(x)<-value
setAttribs(x,\dots)
\method{setAttribs}{character}(x,\dots)
\method{setAttribs}{html_elem}(x,\dots)
\method{setAttribs}{html_group}(x,\dots)

css(\dots)
as.css(x)
style(x)
style(x) <- value
setStyle(x,\dots)
\method{setStyle}{character}(x,\dots)
\method{setStyle}{html_elem}(x,\dots)
\method{setStyle}{html_group}(x,\dots)
}
\arguments{
  \item{tag}{a character string that determines the opening and closing
      tags of the HTML element. (The closing tag is relevant only if
      the element has a content.)}
  \item{\dots}{optional further arguments, named or not.
      
      For \code{html}: named arguments create the attributes of the HTML element,
      unnamed arguments define the content of the HTML element, i.e. whatever 
      appears between opening and closing tags (e.g. \code{<p>} and \code{</p>}).
      Character strings, \code{"html_elem"}, or \code{"html_group"} objects can appear
      as content of a HTML element.
      
      For \code{setAttribs}: named arguments create the attributes of the HTML element,
      unnamed arguments are ignored.
      
      For \code{setStyle}: named arguments create the styling of the HTML element,
      unnamed arguments are ignored.
      
      For \code{html_group}: several objects of class \code{"html_elem"}
      or \code{"html_group"}.
      
      For \code{css}: named arguments (character strings!) 
      become components of a styling in CSS format.
      }
  \item{.content}{an optional character string, \code{"html_elem"}, or 
      \code{"html_group"} object}
  \item{linebreak}{a logical value or vector of length 2, determines
    whether linebreaks are inserted after the HTML tags.}
  \item{x}{an object. For \code{as.html_group}, this should be a list
      of objects of class \code{"html_elem"} or \code{"html_group"}. 
      For \code{content}, \code{setContent}, \code{attribs}, 
      \code{setAttribs}, \code{style}, \code{setStyle},
      this should be an object of class \code{"html_elem"} or \code{"html_group"}.  
  }
  \item{value}{an object of appropriate class. 
  
      For \code{content<-}: a character string, \code{"html_elem"}, or 
      \code{"html_group"} object, or a concatenation thereof.
      
      For \code{attribs<-} or \code{style<-}: a named character vector.
      }
}
\details{
  Objects created with \code{html} are lists with class attribute
  \code{"html_elem"} and components
  \describe{
      \item{tag}{a character string}
      \item{attributes}{a named character vector}
      \item{content}{a character vector, an \code{"html_elem"} or \code{"html_group"}
      object, or a list of such.
        }
      \item{linebreak}{a logical value or vector of length 2.}
  }
  Objects created with \code{html_group} or by concatenation
  of \code{"html_elem"} or \code{"html_group"}
      object
  are lists of such objects, with class attribute \code{"html_group"}.
}
\examples{

html("img")
html("img",src="test.png")
html("div",class="element",id="first","Sisyphus")
html("div",class="element",id="first",.content="Sisyphus")

div <- html("div",class="element",id="first",linebreak=c(TRUE,TRUE))
content(div) <- "Sisyphus"
div

tag <- html("tag",linebreak=TRUE)
attribs(tag)["class"] <- "something"
attribs(tag)["class"]
tag

style(tag) <- c(color="#342334")
style(tag)
tag

style(tag)["bg"] <- "white"
tag

setStyle(tag,bg="black")
setStyle(tag,c(bg="black"))

c(div,tag,tag)

c(
  c(div,tag),
  c(div,tag,tag)
)

c(
  c(div,tag),
  div,tag,tag
)

c(
  div,tag,
  c(div,tag,tag)
)

content(div) <- c(tag,tag,tag)
div


css("background-color"="black",
                  color="white")

as.css(c("background-color"="black",
                  color="white"))



Hello <- "Hello World!"
Hello <- html("p",Hello)
style(Hello) <- c(color="white",
                  "font-size"="40px",
                  "text-align"="center")

Link <- html("a","More examples here ...",
              href="http://elff.eu/software/memisc",
              title="More examples here ...",
              style=css(color="white"))
Link <- html("p"," (",Link,")")
style(Link) <- c(color="white",
                 "font-size"="15px",
                 "text-align"="center")
                  
Hello <- html("div",c(Hello,Link))
style(Hello) <- c("background-color"="#160666",
                  padding="20px")

show_html(Hello)

}
