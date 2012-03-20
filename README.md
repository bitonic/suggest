Simple suggestion and correction server, implemented with warp and using
ternary search tries to complete and correct words.

To find out how it works, check out the 
[slides](https://raw.github.com/rostayob/suggest/master/slides.pdf). The slides
are slightly outdated, since when I wrote them warp still used enumerator, while
now it uses conduits, but the concepts still apply.

I did this the guys at RabbitMQ asked me to give a presentation on a project,
so I coded this in less than 10 hrs the day before (I got the job!).

It's very fast, over 20k req/s.

To build and run:

```
cabal configure ; cabal build
cp dist/build/suggest/suggest ./
./suggest
```

Screenshots:

<img src="https://raw.github.com/rostayob/suggest/master/screenshots/1.png" />
<img src="https://raw.github.com/rostayob/suggest/master/screenshots/2.png" />
