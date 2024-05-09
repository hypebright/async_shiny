# Asynchronous programming in Shiny

This repo contains examples to implement asynchronous programming in Shiny ✨ 

Async programming can sound very daunting but by providing specific Shiny examples the aim is to speed up the learning curve.

## About asynchronous programming

Asynchronous programming allows for the execution of multiple tasks concurrently, improving the performance and responsiveness of Shiny applications. Traditional synchronous programming executes tasks sequentially, which can result in slower response times and blocked resources. By default, a Shiny applications runs on a single-threaded synchronous R session.

Asynchronous programming in Shiny can be achieved through various packages that provide mechanisms for running code concurrently, such as [promises](https://github.com/rstudio/promises/), [callr](https://github.com/r-lib/callr/), [coro](https://github.com/r-lib/coro), [mirai](https://github.com/shikokuchuo/mirai) and [crew](https://github.com/wlandau/crew). These packages allow developers to write more efficient and responsive code.

You can also use `promises` in combination with [ExtendedTask](https://rstudio.github.io/shiny/reference/ExtendedTask.html) from Shiny version 1.8.1.

## Examples

* **promises**: two examples demonstrating cross-session asynchronicity using `promises` and `future`. One example demonstrations inner-session asynchronicity as well by making use of a `reactiveVal()` structure. This follows the examples in [Engineering Production-Grade Shiny Apps](https://engineering-shiny.org/optimizing-shiny-code.html#asynchronous-in-shiny), and it is kind of a workaround. `promises_extendedtask.R` contains an example with `ExtendedTask` from Shiny 1.8.1, which makes it easy to achieve both cross-session and inner-session asynchronicity. It works perfectly in combination with `input_task_button()` and `bind_task_button()` from bslib 0.7.0.
* **callr**: three examples that show how to spin up background R processes. `callr_single_task.R` runs a long computation in the background, `callr_rmd.R` knits a markdown document in the background, and `callr_nonblocking.R` demonstrates how this is not blocking the process by displaying a clock.
* **coro**: one example that demonstrates how to program concurrently using the async() function from `coro`. In this example, we also use `promises` and we demonstrate how `promise_all()` works to handle multiple promises at the same time.
* **mirai**: one example that will walk you through the minimalist package called `mirai` (which means future in Japanese). `mirai` provides a simple interface to efficiently schedule tasks on local or remote resources. `mirai` implements ultra-efficient promises that are completely event-driven and non-polling, and may be used in a Shiny app anywhere that accepts promises. For further examples, including usage with `ExtendedTask`, check out the [mirai Shiny vignette](https://shikokuchuo.net/mirai/articles/shiny.html).
* **crew**: on top of `mirai`, there's a package called `crew` that helps you to manage and control workers. The interface is pretty neat and intuitive. There's a lot of functionality, which includes the option to specify remote workers. In this folder there are two examples: one that demonstrates how to send a single task to a worker, and one that demonstrates how to send multiple tasks to multiple workers. For more info see [crew](https://github.com/wlandau/crew) on GitHub. There are three examples: `crew_single_task.R`, `crew_multiple_tasks.R`, and `crew_nonblocking.R`, where the latter demonstrates the non-blocking behavior of `crew` clearly.

## YouTube 🎥

I have a couple of videos on YouTube about async programming in Shiny:

* [Launch multiple asynchronous tasks in R Shiny with crew + create dynamic number of outputs](https://www.youtube.com/watch?v=udHK5XVSrlE&t=89s)
* [Async Programming in Shiny with crew and callr](https://www.youtube.com/watch?v=DTMVzK7iZFU)
* [Say goodbye to unnecessary waiting: mastering asynchronous programming in Shiny - ShinyConf2023 Keynote Talk](https://www.youtube.com/watch?v=hltOgAC2mC4&t=821s)

## Future plans

The plan is to add more examples and tutorials on how to implement asynchronous programming in Shiny. Keep an eye on this repository for updates.

## Contributors 📣 

Do you want to add examples to this repo? That's awesome 👏 . I'm welcoming all support!
