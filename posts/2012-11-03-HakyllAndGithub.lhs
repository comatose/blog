---
title: Hakyll과 Github로 Blog 만들기
author: comatose
tags: hakyll, blog, github
---

[Github]는 일반적으로 소스저장소로 사용하지만, [Pages]가 지원되면서 간단한 웹서버로 사용 가능하다.
다만, html만 지원되기 때문에, [Jekyll]같은 static site generator도 함께 지원한다.
[Jekyll]을 써도 좋지만, haskell 기반의 [Hakyll]이 더 재밌을 것 같아 [Hakyll]을 사용해 봤다.

일단 [Hakyll]은 생각보다 사용하기 쉬웠는데, tagblog template 기반에 source code 표시를 위한 css만 추가했다.
결과물은 [여기](https://github.com/comatose/blog/tree/gh-pages).

_site 디렉토리를 제대로 인식하기 위해 .nojekyll을 추가하고 index.html에 _site로 redirection하도록 하면 완성.
markdown과 literate haskell로 작성하는게 어색하지만 나쁘지 않은 것 같다.

[Github]:http://www.github.com/
[Pages]:http://pages.github.com/
[Hakyll]:http://jaspervdj.be/hakyll/
[Jekyll]:http://jekyllrb.com/