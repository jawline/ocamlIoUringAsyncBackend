## Async IO uring

An attempt to build an IO uring scheduler using Async. This is a very simple implementation of an Async scheduler using IO uring. It draws on Async_kernel but does not include any of the features of Async_unix. As this is a POC / prototype I do not intend for it to become API compatible with Async.

### So Far;

- Async_command clone so Command can be used
- Basic scheduler
- File IO demo working
- Async vs Io_uring file size check tester (Shows 100% improvement in a hot cache)
- Basic TCP support (reader only for now)
