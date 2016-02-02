setClassUnion("atomic",c("raw","logical","integer","numeric","complex","character"))
setClassUnion("atomic or NULL",c("atomic","NULL"))
setClassUnion("atomic or factor",c("atomic","factor"))
setClassUnion("character or NULL",c("character","NULL"))
setClassUnion("character or integer",c("character","integer"))
# setClassUnion("index",c("logical","numeric","character"))
# setClassUnion("strict.index",c("logical","numeric"))

setClass("value.labels",
  contains = "character",
  representation(
    values = "atomic"
  )
)

setClass("value.filter",
  representation(
    filter="atomic or NULL"
  )
)
setClass("valid.values",contains="value.filter")
setClass("valid.range",contains="value.filter")
setClass("missing.values",contains="value.filter",representation(range="atomic or NULL"))

setClassUnion("value.labels or NULL",c("value.labels","NULL"))
setClassUnion("value.filter or NULL",c("value.filter","NULL"))

setClass("annotation",contains="character")

setClass("item",
  representation(
    value.labels="value.labels or NULL",
    value.filter="value.filter or NULL",
    measurement="character or NULL",
    annotation="annotation"
  )
)

setClass("double",
  contains="numeric",
  prototype=double()
)


setClassUnion("numeric.item")
setClassUnion("item.vector","numeric.item")

setClass("integer.item",
  contains=c("item","numeric.item","item.vector","integer")
)

setClass("double.item",
  contains=c("item","numeric.item","item.vector","double","numeric")
)
setClass("character.item",
  contains=c("item","item.vector","character")
)

setClass("Date.item",
  contains=c("item","numeric.item","item.vector","double","numeric")
)

setClass("datetime.item",
   representation(tzone="character or NULL",origin="character or NULL"),
   contains=c("item","numeric.item","item.vector","double","numeric")
 )

setClass("named.list",
  contains="list",
  representation(
  names = "character")
)

setClass("item.list",contains="named.list")

setClass("data.set",
  contains=c("item.list","named.list","list"),
  representation(
    row_names = "character or integer",
    document = "character"
  )
)

setClass("importer",
  contains=c("item.list","named.list","list"),
  representation(
    ptr="externalptr",
    document = "character"
  )
)


setClass("spss.fixed.importer",
  contains="importer",
  representation(
    columns.file="character",
    varlab.file="character or NULL",
    codes.file="character or NULL",
    missval.file="character or NULL",
    data.spec="list"
  )
)

setClass("spss.portable.importer",
  contains="importer",
  representation(
    varlab.file="character or NULL",
    codes.file="character or NULL",
    missval.file="character or NULL",
    data.spec="list"
  )
)

setClass("spss.system.importer",
  contains="importer",
  representation(
    varlab.file="character or NULL",
    codes.file="character or NULL",
    missval.file="character or NULL",
    data.spec="list"
  )
)

setClass("Stata.importer",
  contains="importer",
  representation(
    data.spec = "list"
  )
)

setClass("codebook",
  contains="list"
)

setClass("codebookEntry",
  representation(
    spec = "character",
    stats = "list",
    annotation = "character or NULL"
  )
)