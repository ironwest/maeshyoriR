# purrr map を使った前処理のアイデア---------------------
library(tidyverse)

#使いどころその1:データインポート

#mapを使う時はまず、繰り返し処理
#CSVファイルを一つ読み込む---------------------------

d19 <- vroom::vroom("data/honhyo_2019.csv",locale = locale(encoding="shift-jis"))
d20 <- vroom::vroom("data/honhyo_2020.csv",locale = locale(encoding="shift-jis"))

#この二つのファイルの列名は、完全に一致している
all(colnames(d19) %in% colnames(d20))

#ただし、縦に繋ごうとすると、
bind_rows(d19,d20)

#型が違う！と怒られます。これは単純に読み込み時に全ての型を揃えて
#置けば問題ないのですが、どこがどう違うかを確認したいとしましょう。
#実は、tibbleやdata.frameは、「列」が一つの要素となったリストです。

typeof(d19)

#なので、mapにtibbleを入れてあげると、

map(d19,~{.[1:3]})

#こんな感じで、各行のデータが「.」に含まれて繰り返し処理をすることができます
#なので、全ての列の型を調べたいような場合は、

map(d19,~{typeof(.)})

#とすると、こんな感じになります。
#また、mapはリストで返ってくるのが基本ですが、リストでない
#形で結果を返すことも可能で、例えば、文字型のベクトルで今回
#の結果を返したい場合は、map_chrを利用します。

d19_type <- map_chr(d19,~{typeof(.)})
d20_type <- map_chr(d20,~{typeof(.)})

#この二つの名前付きベクトルを利用することで、

tibble(
  name = names(d19_type),
  d19t = d19_type,
  d20t = d20_type
) %>% 
  filter(d19t != d20t)

#ということで、d19では、経度と緯度が数値型で読み込まれていたのに、
#d20では、文字型で読み込まれていたため、bind_rowsができなかった
#ということが読み取れました。これは、

d19 <- vroom::vroom("data/honhyo_2019.csv",locale = locale(encoding="shift-jis"))
d20 <- vroom::vroom("data/honhyo_2020.csv",locale = locale(encoding="shift-jis"), 
                    col_types = list(`地点　緯度（北緯）` = "n",
                                     `地点　経度（東経）` = "n"))

dat1 <- bind_rows(d19, d20)

dat1

#として無事bindrowsができました。
#
#さて、ここでvroomでcsvを読み込む処理ですが、より簡潔にすることができます。
#現在、d19,d20と二つの変数を作成しておりますが、もし、dataフォルダにある
#全てのcsvを結合するなどの処理を行いたい場合は、

dat2 <- tibble(x = list.files("data", full.names = TRUE, pattern = "csv")) %>% 
  
  mutate(d = map(x, ~{
    vroom::vroom(
      file   = ., 
      locale = locale(encoding="shift-jis"),
      col_types = list(`地点　緯度（北緯）` = "n",
                       `地点　経度（東経）` = "n"))
  }))

dat2

#としてあげることで、d列の各行に表を含んだ形になります。
#この表は、リストなので、

dat2 <- bind_rows(dat2$d)

#とすることで、表になります。
dat2

#あるいは、map関数はmap_xxxというリスト以外の結果を返す関数があります。
#例えば、次はリストが返ります。

map(c(1,2,3,4), ~{.})

#mapの代わりに、map_xxxを利用すると、

map_chr(c(1,2,3,4,5,6), ~{ letters[.]   })
map_dbl(c(1,2,3,4,5,6), ~{ . * 10       })
map_int(c(1,2,3,4,5,6), ~{ as.integer(.)})
map_lgl(c(1,2,3,4,5,6), ~{ . %% 2 == 0 })

#となり、これ以外に、map_dfr, map_dfcというものもあり、
#dfrは、map関数の結果を縦につなげたものを、

map_dfr(c(1,2,3,4,5,6), ~{
  tibble(a = rep(.,2))
})

#（これは、
bind_rows(
  tibble(a = rep(1,2)),
  tibble(a = rep(2,2)),
  tibble(a = rep(3,2)),
  tibble(a = rep(4,2)),
  tibble(a = rep(5,2)),
  tibble(a = rep(6,2))
) #と同じ）


map_dfc(c(1,2,3,4,5,6), ~{
  dd <- tibble(a = letters[.:(.+3)])
  dd <- setNames(dd,str_c("c",.))
  return(dd)
})

#（これは、
bind_cols(
  tibble(c1 = letters[1:(1+3)]),
  tibble(c2 = letters[2:(2+3)]),
  tibble(c3 = letters[3:(3+3)]),
  tibble(c4 = letters[4:(4+3)]),
  tibble(c5 = letters[5:(5+3)]),
  tibble(c6 = letters[6:(6+3)])
) #と同じ）

#ということで、csvデータの読み込みも、縦につなげてエラーが生じない
#事が確認できている場合は、

dat3 <- map_dfr(
  .x = list.files("data",full.names = TRUE, pattern = "csv"),
  .f = ~{
    vroom::vroom(
      file   = ., 
      locale = locale(encoding="shift-jis"),
      col_types = list(`地点　緯度（北緯）` = "n",
                       `地点　経度（東経）` = "n"))
  }
)

dat3
#とするだけでOKです。
#
#この場合の落とし穴は、

map(
  c(1,2,3),
  ~{
    temp <- tibble(a = rep(.,3), b = letters[.])
    if(. == 2) temp <- temp %>% mutate(a = as.character(a))
    return(temp)
  }
)

#[[2]]の内容をみてみると、列aがこれだけ文字型になっています
#（と言いますか、2の時だけ意図的に文字型になるように作っています）
#こういう結果が含まれてしまっていると、map_dfrをするとエラーに
#なります。

map_dfr(
  c(1,2,3),
  ~{
    temp <- tibble(a = rep(.,3), b = letters[.])
    if(. == 2) temp <- temp %>% mutate(a = as.character(a))
    return(temp)
  }
)

#この例であれば、すぐに処理が終わるのでよいのですが、
#これが、map全体の処理に数分かかる場合、最終的にはエラーとなって
#しまい、かなり効率が悪いので、

res <- map(
  c(1,2,3),
  ~{
    temp <- tibble(a = rep(.,3), b = letters[.])
    if(. == 2) temp <- temp %>% mutate(a = as.character(a))
    return(temp)
  }
)

res <- res %>% bind_rows()

#としてあげて、処理の終了を優先させて、bind_rowsでエラーが生じたら、
res2 <- tibble(pre = res) %>% 
  mutate(post = map(res, ~{
    temp <- .
    temp %>% 
      mutate(a = as.numeric(a)) %>% 
      return()
  }))

bind_rows(res2$post)

#と、後からbind_rowsで結合できない理由を探して、
#処理をしてしまうことが多いです。
#以上、インポートで使うmapでした。

#----------------------------
# 使いどころその2:列の加工をmap、imap、map_dfcでやってみる。

# 実演していきます。
# とりあえず、読み込んだオープンデータ
dat3

#の列名が多いので、列を絞り込んで、かつ、列名を英語にしておきましょう。
#（今日の話はあくまでデータの加工に焦点を絞っているため、意味のある
#分析や集計を行う意図はありません）

dat4 <- dat3 %>% 
  select(pref        = `都道府県コード`,
         jiko_naiyou = `事故内容`,
         death_num   = `死者数`,
         injured_num = `負傷者数`,
         date_year   = `発生日時　　年`,
         date_month  = `発生日時　　月`,
         date_day    = `発生日時　　日`,
         hiruyoru    = `昼夜`,
         weather     = `天候`,
         romen       = `路面状態`,
         ido         = `地点　緯度（北緯）`,
         keido       = `地点　経度（東経）`)
    
#日付を日付時刻型の列:acc_dateとして作成。date_year等は削除
dat5 <- dat4 %>% 
  mutate(
    acc_date = lubridate::make_date(
      year = date_year,
      month = date_month,
      day = date_day
    )
  ) %>% 
  select(!c(date_year, date_month, date_day)) %>% 
  relocate(acc_date)

#death_num, injured_num, ido, keidoを適切な数字に置き換える。
dat6 <- dat5 %>% 
  mutate(across(c(death_num, injured_num), ~{as.numeric(.)}))

#pref, jiko_naiyou, hiruyoru, weather, romenの5列は
#data/codebook_2020.pdfに掲載されている内容を下に因子型に変換
#（pdfから表を取り出す工程は今回は省略:extract_from_codebook.Rに
#内容はあるので興味のある方は確認してみてください。）

source("extract_from_codebook.R", encoding="utf-8")
#このスクリプトで、pdfの表から、今回利用する5変数分の「対応表」
#がdという名前の変数に収納されています。

d

#項目に項目名、tbsに表が含まれており、内容を確認するには、

d$tbs[[1]]
#等とするとOKです。各行に、このような、コードとそれに対応する
#値が含まれた表がはいっています。
#
#この、dを利用して、dat6の変数のコードを因子型に置き換えてみましょう。
#ここでは、いくつかやり方がありますが、tibbleが列毎のリストであるという
#ことを利用して、mapを利用してみます。
#このとき、map2を次のように使うと、

map2(dat6, names(dat6), ~{
  print(.y)
  print(.x[1:5])
})

#列名と、その値をmapの中で利用することができます。なので、
#列名がXXXXのとき、OOOOOとした処理をした列を作成する
#という処理を行う場合は、

dat7_1 <- map2(dat6, names(dat6),~{
  
  #dから列名に応じた行を抜き出す。
  single_code <- d %>% filter(koumoku2 == .y)

  if(nrow(single_code)==0){
    #もしdに対応する変換表がなければ、単純な
    #1列のtibbleを作成する。
    
    res <- tibble(x = .x) %>% setNames(.y)
  
  }else{
    #もしdに対応する変換表があれば、変換表を利用して、
    #因子型の列を持つtibbleを作成する。
    
    code_table <- single_code$tbs[[1]]
    
    flevel <- code_table %>% pull(1)
    flabel <- code_table %>% pull(2)
    
    res <- tibble(x = .x) %>% 
      mutate(x = factor(x, levels = flevel, labels = flabel)) %>% 
      setNames(.y)
  }
  
  return(res)
})

dat7_1 #と、こんな感じです。

#このmapの中身の処理を関数にしておきます。
apply_codebook <- function(vec, name, d){

  single_code <- d %>% filter(koumoku2 == name)
  
  tib <- tibble(x = vec)
  
  if(nrow(single_code)==0){
    res <- tib
  }else{
    code_table <- single_code$tbs[[1]]
    
    flevel <- code_table %>% pull(1)
    flabel <- code_table %>% pull(2)
    
    res <- tib %>% 
      mutate(x = factor(x, levels = flevel, labels = flabel))
  }
  
  res <- res %>% setNames(name)
  
  return(res)
}

dat7_1 <- map2(dat6, names(dat6), ~{
  apply_codebook(.x, .y, d)
})

#ここで、dat7_1は1列のtibbleがリストの中に順番に含まれている状態
#なのですが、map2_dfcを利用すると、それを横方向に並べた
#tibbleとして結果を出力することができるので、

dat7_2 <- map2_dfc(dat6, names(dat6),~{
  apply_codebook(.x, .y, d)
})

#こうなりました。
dat7_2

#さらに、purrrには、map2(dat,names(dat),~{})をより簡単に書くことが
#できる関数があるので、

imap(dat6, ~{
  apply_codebook(.x, .y, d)
})

imap_dfc(dat6, ~{
  apply_codebook(.x, .y, d)
})

#こんな感じでかくことができました。
#頻繁には使わないかもしれませんし、across関数を利用することの
#方が多いかもしれませんが、imapを利用した表データの列の繰り返し処理と
#_dfcを利用した表の作成は、結構力任せになりますが、列の加工ができますの
#で、使えるようになると便利な場合もあるかもしれません。


#その3―mapを使ってアウトプットを出力する-----
#本日ご紹介する最後の使い方です。
#

dat <- dat7_2

#厳密には前処理とは少し違うかもしれませんが、
#グラフと回帰分析の結果を複数作成する方法について解説してみます。

#このデータ、例えば、大阪に限定すると、

# ちょっと加工して
osaka <- dat %>% 
  filter(pref == "大阪") %>% 
  mutate(yr = lubridate::year(acc_date)) %>% 
  group_by(yr) %>% 
  arrange(acc_date) %>% 
  mutate(cumdeath = cumsum(death_num)) %>% 
  mutate(is_death = death_num > 0)

#事故件数のグラフを書いて
ggplot(osaka) +
  geom_bar(aes(x = acc_date, fill = hiruyoru)) +
  facet_wrap(~romen) +
  labs(title = "事故件数") +
  theme_bw()

#累積死亡数のグラフを書いて
ggplot(osaka) +
  geom_col(aes(x = acc_date, y = cumdeath)) +
  facet_wrap(~yr, scales = "free_x") +
  labs(y = "累積死亡数", title = "年毎の累積死亡数") +
  theme_bw()

#死亡事故の有無でのロジスティック回帰分析を行って
model <- glm(is_death ~ hiruyoru + weather + romen, family = binomial(link = "logit"), data = osaka)
modelres <- summary(model)

#ということを、全都道府県にやりたい場合を考えます。
#これを、forループではなく、map関数を使ってやります。
#
#まず、上のデータ加工とグラフ、モデルの作成を関数として作成しておきます。

syori_dat <- function(tgtpref, dat){
  prefdat <- dat %>% 
    filter(pref == tgtpref) %>%  
    mutate(yr = lubridate::year(acc_date)) %>% 
    group_by(yr) %>% 
    arrange(acc_date) %>% 
    mutate(cumdeath = cumsum(death_num)) %>% 
    mutate(is_death = death_num > 0)
  
  return(prefdat)
}

gen_g1 <- function(tgtpref, prefdat){
  ggplot(prefdat) +
    geom_bar(aes(x = acc_date, fill = hiruyoru)) +
    facet_wrap(~romen) +
    labs(title = str_c(tgtpref,"の事故件数")) +
    theme_bw()
}

gen_g2 <- function(tgtpref, prefdat){
  ggplot(prefdat) +
    geom_col(aes(x = acc_date, y = cumdeath)) +
    facet_wrap(~yr, scales = "free_x") +
    labs(y = "累積死亡数", title = str_c(tgtpref,"の年毎の累積死亡数")) +
    theme_bw()
}

gen_model <- function(prefdat){
  model <- glm(is_death ~ hiruyoru + weather + romen, family = binomial(link = "logit"), data = prefdat)

  return(model)
}

#これら4つの関数を全ての都道府県の区別
dat %>% count(pref)

# の、51個の分類毎に適応してみます。
# これは、それほど難しいものではなく

ndat <- dat %>% 
  group_by(pref) %>% 
  nest()

#としてあげると、
ndat

#は、こんな感じで、data列は、一つの行に、例えば、6行目
#青森であれば、5227行×9列のtibbleが含まれている形になります。
#先の例の「osaka」データに該当するのが、このdata列の1行1行で、
#このデータに対して、作成した関数を適応することで、
#加工したデータ、グラフ2つ、モデル1つを全てのprefの値に対して
#簡単に作成することができます。
#
#じっさいにやってみましょう。

final <- ndat %>% 
  mutate(
    procdat = map(pref,           ~{syori_dat(.,dat)}),
    g1      = map2(pref, procdat, ~{gen_g1(.x,.y)}),
    g2      = map2(pref, procdat, ~{gen_g2(.x,.y)}),
    mod     = map(procdat       , ~{tryCatch(gen_model(.),error=function(e)e)})
  )

#出来上がりました。これは
final

#な感じで、g1g2mod列それぞれに、ggplotのグラフ二つと、モデル一つが含まれています。
#これ、例えば、10番の山形の結果を確認したい場合は、

yamagata <- final %>% filter(pref == "山形")

yamagata$procdat[[1]]
yamagata$g1[[1]]
yamagata$g2[[1]]
yamagata$mod[[1]]

#このように取り出すことができました。

#この使い型、「核」となる要素をうまく
#取り出すこと、指定することができたら、map関数を利用して、
#様々な結果を一つの表オブジェクトの中に保持することができます

#ひとつひとつ取り出す他にも、
#モデルから係数とP値を取り出して都道府県別の結果のグラフを
#作成するようなことも可能です。
#
#例えば、天気による死亡事故の有無を都道府県でのデータ別に
#取り出して見てあげたいような場合は、

final$mod[[1]] %>% 
broom::tidy() %>% 
  filter( str_detect(term,"weather"))

#を都道府県別に実施してあげて

final %>% 
  mutate(weather = map(mod, ~{
    broom::tidy(.) %>% filter(str_detect(term,"weather")) %>% return()
  }))
  
#その結果を表にして、

final %>% 
  mutate(weather = map(mod, ~{
    broom::tidy(.) %>% filter(str_detect(term,"weather")) %>% return()
  })) %>% 
  select(pref, weather) %>% 
  unnest(weather)

#さらにtermが雨の場合の結果、霧の場合の結果、雪の場合の結果にnestした
#tibbleにしてあげて、

final2 <- final %>% 
  mutate(weather = map(mod, ~{
    broom::tidy(.) %>% filter(str_detect(term,"weather")) %>% return()
  })) %>% 
  select(pref, weather) %>% 
  unnest(weather) %>% 
  group_by(term) %>% 
  nest()

#nestしたdata列一つをグラフにするには、
final2$data[[1]] %>% 
  ggplot() +
  geom_col(aes(x = exp(estimate), y = reorder(pref,estimate), fill = p.value<0.05)) +
  labs(x = "OR", y = "都道府県")

#このようにすればよいので、これをfinal2に適応してあげると、
final2 <- final2 %>% 
  mutate(gg = map2(data,term, ~{
    ggplot(.x) +
      geom_col(aes(x = exp(estimate), y = reorder(pref,estimate), fill = p.value<0.05)) +
      labs(x = "OR", y = "都道府県", title = .y)
  }))

final2

#あとは、gg列に含まれるグラフをcowplotなどでまとめると、

cowplot::plot_grid(plotlist = final2$gg)

#こんな結果になりました。
#（分析内容については、今回、モデルの適合等を
#検証しているわけではないので、あくまで「こういうこともできる」
#という見本のような形で結果については無視していただけると幸いです）


#mapについて、以上となります。最初はわかりにくいことも
#多いかもしれませんが、使っていくうちにリストを
#mapで操作する感覚がつかめるようになると思います。
#
#ご清聴ありがとうございました！




#宣伝：
# Udemyの動画へのクーポンを含むリンク（ブログサイト）:
# 　　https://r-online-course.netlify.app/#hero

# 書籍：Rでらくらくデータ分析入門：
#　　技術評論社Ｗｅｂページ：https://gihyo.jp/book/2022/978-4-297-12514-1
#　　アマゾン：https://amzn.to/3OvMHER

# purrr::mapについて解説した過去のブログ記事
#https://r-online-course.netlify.app/post/2021-09-30-use-maps-in-tibble/
