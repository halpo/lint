{############################################################################### 
# base.patterns.R
# This file is part of the R package lint.
# 
# Copyright 2012 Andrew Redd
# Date: 7/30/2012
# 
# DESCRIPTION
# ===========
# Base Patterns for building off.
# 
# LICENSE
# ========
# lint is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software 
# Foundation, either version 3 of the License, or (at your option) any later 
# version.
# 
# dostats is distributed in the hope that it will be useful, but WITHOUT ANY 
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along with 
# this program. If not, see http://www.gnu.org/licenses/.
# 
}###############################################################################



start.characters <- "[\\p{L}\\.]"
following.characters <- "[\\p{L}\\p{N}\\._\\d]"
name.pattern <- sprintf("%s%s*", start.characters, following.characters)
real.constant <- c("\\d+\\.?\\d*", "\\d*\\.?\\d+")
exp.constant <- "\\d+\\.?\\d*[eE][+-]?\\d+"
int.constant <- "\\dL"
complex.constant <- sprintf("%si", real.constant)
numeric.constant.all <- c(real.constant, exp.constant, int.constant
                          , complex.constant)
numeric.constant <- paste0('(', paste(numeric.constant.all, collapse='|'), ')')
