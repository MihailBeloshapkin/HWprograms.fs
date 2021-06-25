module Factory

open ILazy
open SimpleLazy
open ParallelLazy
open LockFreeLazy

type LazyFactory =
    static member CreateSimpleLazy supplier =
        new SimpleLazy<'a>(supplier) :> ILazy<'a>

    static member CreateLockFreeLazy supplier =
        new LockFreeLazy<'a>(supplier) :> ILazy<'a>

    static member CreateParallelLazy supplier =
        new ParallelLazy<'a>(supplier) :> ILazy<'a>

