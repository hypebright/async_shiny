# Asynchronous programming in Shiny

This repo contains examples to implement asynchronous programming in Shiny ✨ 

Async programming can sound very daunting but by providing specific Shiny examples the aim is to speed up the learning curve.

## About asynchronous programming

Asynchronous programming allows for the execution of multiple tasks concurrently, improving the performance and responsiveness of Shiny applications. Traditional synchronous programming executes tasks sequentially, which can result in slower response times and blocked resources. By default, a Shiny applications runs on a single-threaded synchronous R session.

Asynchronous programming in Shiny can be achieved through various packages that provide mechanisms for running code concurrently, such as [promises](https://github.com/rstudio/promises/), [callr](https://github.com/r-lib/callr/), and [coro](https://github.com/r-lib/coro). These packages allow developers to write more efficient and responsive code.

## Examples

* **promises**: two examples demonstrating cross-session asynchronicity using `promises` and `future`. One example demonstrations inner-session asynchronicity as well by making use of a `reactiveVal()` structure. This follows the examples in [Engineering Production-Grade Shiny Apps](https://engineering-shiny.org/optimizing-shiny-code.html#asynchronous-in-shiny).
* **callr**: two examples that show how to spin up background R processes. The basic example runs a long computation in the background, and the markdown example knits a markdown document in the background.
* **coro**: one example that demonstrates how to program concurrently using the async() function from `coro`. In this example, we also use `promises` and we demonstrate how `promise_all()` works to handle multiple promises at the same time. 

## Future plans

The plan is to add more examples and tutorials on how to implement asynchronous programming in Shiny. Keep an eye on this repository for updates.

## Watch "Mastering Asynchronous Programming in Shiny"

Coming soon! 👀 You can watch the keynote talk on the ShinyConf2023 back on the Appsilon YouTube channel 🎥

The [slides of the presentation](http://hypebright.nl/wp-content/uploads/2023/04/VeerlevanLeemput-ShinyConf2023-20230317v2.pdf) are already available!
