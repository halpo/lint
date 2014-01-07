only1 <- function(.list){
    all(.list==.list[1])
}
is_unity <- function(...)only1(list(...))
is_homo <- function(.list){
    classes <- lapply(.list, class)
    only1(classes)
}
is_homogeneous <- function(...)is_homo(list(...))

