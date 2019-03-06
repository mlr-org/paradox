param_set = paradox::ParamSet$new(
          params = list(
            paradox::ParamInt$new(id = "minsplit", default = 20L, lower = 1L, tags = "train"),
            paradox::ParamDbl$new(id = "cp", default = 0.01, lower = 0, upper = 1, tags = "train"),
            paradox::ParamInt$new(id = "maxcompete", default = 4L, lower = 0L, tags = "train"),
            paradox::ParamInt$new(id = "maxsurrogate", default = 5L, lower = 0L, tags = "train"),
            paradox::ParamInt$new(id = "maxdepth", default = 30L, lower = 1L, upper = 30L, tags = "train"),
            paradox::ParamInt$new(id = "xval", default = 10L, lower = 0L, tags = "train")
          )
        )

param_set
