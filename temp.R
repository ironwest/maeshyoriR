library(tidyverse)

#pdfデータの読み込み
p <- pdftools::pdf_text("data/codebook_2019.pdf")

d <- tibble(p = p) |>
  mutate(koumoku = map_chr(p,~{
    str_trim(str_extract(.,"(?<=項目名).+(?=\\n)"))
  })) |>
  fill(koumoku)

d <- d |>
  filter(
    koumoku %in% c(
      "都道府県コード","昼夜","天候","地形","路面状態"
    )
  )

codes <- list()

tbs <- map(c(1:5), ~{
  i <- .

  t <- d$p[[i]] %>%
    str_split("\\n") %>%
    {.[[1]]} %>%
    tibble(x = .) %>%
    mutate(tbpart = cumsum(str_detect(x,"^コード"))) %>%
    filter(tbpart==1) %>%
    select(x)

  colns <- t %>%
    slice(1) %>%
    pull(x) %>%
    str_split("\\s+") %>%
    {.[[1]]}

  if(i %in% c(1,2,5)){
    t <- t %>%
      slice(2:nrow(.)) %>%
      mutate(x = str_trim(x)) %>%
      separate(x,colns, sep="\\s+",fill = "right") %>%
      filter(!is.na(!!rlang::sym(colns[length(colns)])))

  }else{
    t <- t %>%
      slice(2:nrow(.)) %>%
      mutate(x = str_trim(x)) %>%
      separate(x,colns, sep="\\s+",fill = "right") %>%
      filter(str_detect(`コード`,"^\\d+$")) %>%
      filter(!is.na(`区分`))
  }


  return(t)
})

d <- d %>%
  mutate(tbs = tbs) %>%
  select(koumoku,tbs)



