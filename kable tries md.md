---
title: kable tries md.md
output:
  pdf_document: default
  html_document: default
---


Collection of tries for tables in Markdown
==========================================

* Classic

```
| Col 1| Col 2| Col 3| Col 4|
|-----:|-----:|-----:|-----:|
|     1|     4|     7|    10|
|     2|     5|     8|    11|
|     3|     6|     9|    12|

```

| Col 1| Col 2| Col 3| Col 4|
|-----:|-----:|-----:|-----:|
|     1|     4|     7|    10|
|     2|     5|     8|    11|
|     3|     6|     9|    12|



* Empty corner

```
|  | Col 2| Col 3| Col 4|
|-----:|-----:|-----:|-----:|
|      |     4|     7|    10|
|     2|     5|     8|    11|
|      |     6|     9|    12|
```

|  | Col 2| Col 3| Col 4|
|-----:|-----:|-----:|-----:|
|      |     4|     7|    10|
|     2|     5|     8|    11|
|      |     6|     9|    12|



* With One corner header elt (and no other)

```
|  SOOO LOOONG AAA TITLE    |
|:-----|-----:|-----:|-----:|
|     1|     4|     7|    10|
|     2|     5|     8|    11|
|     3|     6|     9|    12|
```

|  SOOO LOOONG AAA TITLE    |
|:-----|-----:|-----:|-----:|
|     1|     4|     7|    10|
|     2|     5|     8|    11|
|     3|     6|     9|    12|


* Trying an extra title row (nope, does'nt work)

```
|  SOOO LOOONG AAA TITLE    |
| Col 1| Col 2| Col 3| Col 4|
|:-----|-----:|-----:|-----:|
|     1|     4|     7|    10|
|     2|     5|     8|    11|
|     3|     6|     9|    12|
```
|  SOOO LOOONG AAA TITLE    |
| Col 1| Col 2| Col 3| Col 4|
|:-----|-----:|-----:|-----:|
|     1|     4|     7|    10|
|     2|     5|     8|    11|
|     3|     6|     9|    12|


* Trying an extra title row (2nd tack) (hmm... sort of...)

```
|  SOOO LOOONG AAA TITLE    |
|-----:|-----:|-----:|-----:|
| Col 1| Col 2| Col 3| Col 4|
|------|------|------|------|
|     1|     4|     7|    10|
|     2|     5|     8|    11|
|     3|     6|     9|    12|
```
|  SOOO LOOONG AAA TITLE    |
|-----:|-----:|-----:|-----:|
| Col 1| Col 2| Col 3| Col 4|
|------|------|------|------|
|     1|     4|     7|    10|
|     2|     5|     8|    11|
|     3|     6|     9|    12|


* Dividing horizontal line (nope, not really)

```
| Col 1| Col 2| Col 3| Col 4|
|:-----|-----:|-----:|-----:|
|     1|     4|     7|    10|
|     2|     5|     8|    11|
|     3|     6|     9|    12|
|------|------|------|------|
|     3|     6|     9|    12|
```
| Col 1| Col 2| Col 3| Col 4|
|:-----|-----:|-----:|-----:|
|     1|     4|     7|    10|
|     2|     5|     8|    11|
|     3|     6|     9|    12|
|------|------|------|------|
|     3|     6|     9|    12|


* suspension points end-o-columnd

```
| Col 1| Col 2| Col 3| Col 4|
|:-----|-----:|-----:|-----:|
|     1|     4|     7|    10|
|     2|     5|     8|    11|
|   ...|   ...|   ...|   ...|
```
| Col 1| Col 2| Col 3| Col 4|
|:-----|-----:|-----:|-----:|
|     1|     4|     7|    10|
|     2|     5|     8|    11|
|   ...|   ...|   ...|   ...|


* one long corner header

```
| Longcorner| Col 2| Col 3| Col 4|
|:-----|-----:|-----:|-----:|
|     1|     4|     7|    10|
|     2|     5|     8|    11|
|     2|     5|     8|    11|
|     x|     y|     z|    tt|
```

| Longcorner| Col 2| Col 3| Col 4|
|:-----|-----:|-----:|-----:|
|     1|     4|     7|    10|
|     2|     5|     8|    11|
|     2|     5|     8|    11|
|     x|     y|     z|    tt|


* Again

```
| Longcorner| Col 2| Col 3| Col 4|
|:-----|-----:|-----:|-----:|
|     1|     4|     7|    10|
|     2|     5|     8|    11|
```

| Longcorner| Col 2| Col 3| Col 4|
|:-----|-----:|-----:|-----:|
|     1|     4|     7|    10|
|     2|     5|     8|    11|


* Without header (No, does not work)

```
|:-----|-----:|-----:|-----:|
|     1|     4|     7|    10|
|     2|     5|     8|    11|
```

|:-----|-----:|-----:|-----:|
|     1|     4|     7|    10|
|     2|     5|     8|    11|


* Without header (No, does not work)

```
|:-----|-----:|-----:|-----:|
|     1|     4|     7|    10|
|     2|     5|     8|    11|

```

|:-----|-----:|-----:|-----:|
|     1|     4|     7|    10|
|     2|     5|     8|    11|





* very intriguing, works on pdf, not html (Pandoc Markdown!)

```
----  ----  ----  ----
   2     8    11   222
   3     9    10   333
   5     1   655   950        
----  ----  ----  ----
```

----  ----  ----  ----
   2     8    11   222
   3     9    10   333
   5     1   655   950        
----  ----  ----  ----


* Again

```
----  ----  ----  ----
   2     8    11   222
   3     9    10   333
----  ----  ----  ----
   2     8    11   222
   3     9    10   333
----  ----  ----  ----
```

----  ----  ----  ----
   2     8    11   222
   3     9    10   333
----  ----  ----  ----
   2     8    11   222
   3     9    10   333
----  ----  ----  ----

* more

```
----  ----  ----  ----
   2     8    11   222
----  ----  ----  ----
   3     9    10   333
----  ----  ----  ----
   2     8    11   222
----  ----  ----  ----
   3     9    10   333
----  ----  ----  ----
```

----  ----  ----  ----
   2     8    11   222
----  ----  ----  ----
   3     9    10   333
----  ----  ----  ----
   2     8    11   222
----  ----  ----  ----
   3     9    10   333
----  ----  ----  ----


* Very interesting! http://fletcher.github.io/MultiMarkdown-5/tables.html; AAARGH No!, not in Rstudio? This is MultiMarkdown

```
|             |          Grouping           ||
First Header  | Second Header | Third Header |
 ------------ | :-----------: | -----------: |
Content       |          *Long Cell*        ||
Content       |   **Cell**    |         Cell |

New section   |     More      |         Data |
And more      | With an escaped '\|'         ||
[Prototype table]
```


|             |          Grouping           ||
First Header  | Second Header | Third Header |
 ------------ | :-----------: | -----------: |
Content       |          *Long Cell*        ||
Content       |   **Cell**    |         Cell |

New section   |     More      |         Data |
And more      | With an escaped '\|'         ||
[Prototype table]




*simpler?

```
First Header  | Second Header | Third Header |
 ------------ | :------------ | -----------: |
Content       |               *Long Cell*        ||
Content       |   **Cell**    |         Cell |
New section   |     More      |         Data |
```



First Header  | Second Header | Third Header |
 ------------ | :------------ | -----------: |
Content       |               *Long Cell*        ||
Content       |   **Cell**    |         Cell |
New section   |     More      |         Data |
