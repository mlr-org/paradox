library(paradox)

load_all()

TOKENS = c('NAME', 'NUM', 'BOOL', 'NEWLINE')
LITERALS = c('{','}','[',']', ',')

Lexer <- R6::R6Class("Lexer",
  public = list(
    tokens = TOKENS,
    literals = LITERALS,
    t_NAME = '[a-zA-Z_][a-zA-Z0-9_]*',
    t_NEWLINE = function(re = '\\n+', t) {
      t$lexer$lineno <- t$lexer$lineno + nchar(t$value)
      t$value = NULL
      return(t)
    },
    t_NUM = function(re = "[0-9]*\\.?[0-9]+((E|e)(\\+|-)?[0-9]+)?", t) {
      t$value = as.numeric(t$value)
      return(t)
    },
    t_BOOL = function(re='(T|F)', t) {
      t$value = as.logical(t$value)
      return(t)
    },
    t_ignore = " \t",
    # t_newline = function(re='\\n+', t) {
      # t$lexer$lineno <- t$lexer$lineno + nchar(t$value)
      # return(NULL)
    # },
    t_error = function(t) {
      stopf("Illegal character in PCS format: '%s'", t$value[1])
    }
  )
)

# FIXME: error handling and error messages for invalid sequences seem to be really bad
# FIXME: remove ::: for NO_DEF
# FIXME: add simple dependencies
# FIXME: better errmsg, show current line where lexer is

Parser <- R6::R6Class("Parser",
  public = list(
    tokens = TOKENS,
    literals = LITERALS,
    names = new.env(hash=TRUE),
    p_paramset = function(doc = "paramset : paramlist", p) {
      p$set(1, ParamSet$new(p$get(2)))
    },

    p_paramlist = function(doc = "paramlist : param
                                            | param NEWLINE paramlist", p) {
      if (p$length() == 2L)
        xs = list(p$get(2))
      else if (p$length() == 4L)
        xs = c(list(p$get(2)), p$get(4))
      p$set(1, xs)
    },

    p_param = function(doc = "param : paramint
                                    | paramdbl
                                    | paramfct
                                    | paramlgl" , p) {
      p$set(1, p$get(2))
    },

    p_paramint = function(doc = "paramint : NAME '[' NUM ',' NUM ']' '[' NUM ']' NAME
                                          | NAME '[' NUM ',' NUM ']' '!'" , p) {
      isym = p$get(11)
      if (isym != "i")
          stopf("Final symbol after a numerical param can only be 'i' for an 'int', not: %s", isym)
      def = if (p$length() == 11L) p$get(9) else paradox:::NO_DEF
      param = ParamInt$new(id = p$get(2), lower = p$get(4), upper = p$get(6), default = def)
      p$set(1, param)
    },

    p_paramdbl = function(doc = "paramdbl : NAME '[' NUM ',' NUM ']' '[' NUM ']'
                                          | NAME '[' NUM ',' NUM ']'" , p) {
      def = if (p$length() == 10L) p$get(9) else paradox:::NO_DEF
      param = ParamDbl$new(id = p$get(2), lower = p$get(4), upper = p$get(6), default = def)
      p$set(1, param)
    },

    p_paramlgl = function(doc = "paramlgl : NAME '[' BOOL ']'
                                          | NAME" , p) {
      def = if (p$length() == 5L) p$get(4) else paradox:::NO_DEF
      param = ParamLgl$new(id = p$get(2),  default = def)
      p$set(1, param)
    },

    p_paramfct = function(doc = "paramfct : NAME '{' fct_vallist '}' '[' NAME ']'
                                          | NAME '{' fct_vallist '}'" , p) {
      def = if (p$length() == 8L) p$get(7) else paradox:::NO_DEF
      param = ParamFct$new(id = p$get(2), values = p$get(4), default = def)
      p$set(1, param)
    },

    p_fct_vallist = function(doc = "fct_vallist : NAME
                                                | NAME ',' fct_vallist" , p) {
      if (p$length() == 2L)
        xs = p$get(2)
      else if (p$length() == 4L)
        xs = c(p$get(2), p$get(4))
      p$set(1, xs)
    },

    p_error = function(p) {
      # ppp <<- p
      # n = p$lexer$lineno
      # s = p$lexer$lexdata
      # str_split()
      # print(n)
      if (is.null(p))
        stop("Syntax error parings PCS format at EOF")
      else
        stopf("Syntax error parsing PCS format at '%s'", p$value)
    }
  )
)






lexer  = rly::lex(Lexer)
parser = rly::yacc(Parser)

s = " foo {1, 7] [3] i
      bar [1, 3.4] [2.2]
      b "
x = parser$parse(s, lexer)
print(x)







