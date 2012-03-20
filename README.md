Simple suggestion and correction server, implemented with warp and using
ternary search tries to complete and correct words.

It's very fast, over 20k req/s.

To build and run:

```
cabal configure ; cabal build
cp dist/build/suggest/suggest ./
./suggest
```

Screenshots:
https://raw.github.com/rostayob/suggest/master/screenshots/1.png
https://raw.github.com/rostayob/suggest/master/screenshots/2.png

I did this the guys at RabbitMQ asked me to give a presentation on a project,
so I coded this in less than 10 hrs the day before. This are the slides I used:
https://raw.github.com/rostayob/suggest/master/slides.pdf
