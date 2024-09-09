# CAS CS 320: Concepts of Programming Languages

This is the course repository for CAS CS 320 at Boston University
during the Fall 2024 semester.  We'll publish assignments, lecture
materials and lab materials here.

Follow the instructions below for setting up your machine.  If you are
a Windows user, we generally recommend setting up a [WSL development
environment](https://learn.microsoft.com/en-us/windows/wsl/setup/environment).

# Working with the Course Repository

As a student, you'll mirror this repository and pull down assignments
when they are made available.  You'll also need to link this
repository with Gradescope to submit assignments.

## Mirroring this Repository

0. The following instructions assume that you are set up to connect to GitHub using SSH.
   See the [tutorials](https://docs.github.com/en/authentication/connecting-to-github-with-ssh) in the GitHub docs for more information on how to do this.
   If you don't want to use SSH, then you'll have to replace each repository with its web url, e.g., you should replace
   ```
   git@github.com:BU-CS320/cs320-fall-2024.git
   ```
   with
   ```
   https://github.com/BU-CS320/cs320-fall-2024.git
   ```
   and
   ```
   git@github.com:YOURNAME/cs320-fall-2024-private.git
   ```
   with
   ```
   https://github.com/YOURNAME/cs320-fall-2024-private.git
   ```

1. Clone this repository

   ```
   git clone git@github.com:BU-CS320/cs320-fall-2024.git
   ```

2. Create a **private** repository called `cs320-fall-2024-private` on
   Github, e.g., [https://github.com/nmmull/cs320-fall-2024-private](https://github.com/nmmull/cs320-fall-2024-private)

3. Mirror-push this repository into your private repository

   ```
   git -C ./cs320-fall-2024 push --mirror git@github.com:YOURNAME/cs320-fall-2024-private.git
   ```

4. Clone your private repository

   ```
   git clone git@github.com:YOURNAME/cs320-fall-2024-private.git
   ```

5. Add **this** repository as a remote for your private repository

   ```
   git -C ./cs320-fall-2024-private remote add upstream git@github.com:BU-CS320/cs320-fall-2024.git
   ```
   **Note.** This was recently updated.
   If you set it up with the previous command, you should remove that remote and add this remote instead.
   This means running the following

   ```
   cd cs320-fall-2024-private remote remove upstream
   git add remote upstream git@github.com:BU-CS320/cs320-fall-2024.git
   ```

6. Remove the clone of this repository

   ```
   rm -rf cs320-fall-2024
   ```

You'll complete all the work for the course in your private repository.

## Syncing with the Course Repository

When materials are added to the course repository, you'll need merge
those changes into your private repository. **You should get in the
habit of doing this frequently.**

From within the directory for your private repository, run the
following commands (you should check that you don't have any
uncommitted changes in your directory).

```
cd cs320-fall-2024-private
git fetch upstream
git merge upstream/main main
git push
```

This will merge the updates to the course repository into your private
repository both locally and on Github.

## Submitting Assignments

You should commit your changes frequently in your private repository
(we'll assume familiarity with `git`, see the [git
tutorial](https://git-scm.com/docs/gittutorial) for more information
if you are unfamiliar).  Before you submit an assignment, **make sure
that you've pushed all your changes** using `git push`.  On
Gradescope, you'll be able to choose your private repository for
grading.

# Setting up your Private Repository

After you've mirrored the course repository, you'll set up your
machine to be able to compile and run OCaml program.

1. Install [opam](https://opam.ocaml.org/doc/Install.html).
   Follow all the associated instructions, e.g., make sure to run `opam init`.

2. Create a local switch in the directory for your private repository

   ```
   cd cs320-fall-2024-private
   opam switch create ./ 4.13.1
   eval $(opam env)
   ```

3. Install the `dune`, `utop` and `ounit2`, which we will use throughout the course

   ```
   opam update
   opam install dune utop ounit2
   ```

4. Install the course standard library.  We include
   [documentation](https://nmmull.github.io/CS320/landing/Fall-2024/Specifications/Stdlib320/index.html)
   for this library on the course webpage.  We'll grade assignments
   under the assumption that you only have access to this library (the
   dune projects we release for assignments assume this as well, so
   you don't need to do any additional setup).

   ```
   opam install stdlib320/.
   ```

5. If you are using VSCode, install the OCaml language server protocol

   ```
   opam install ocaml-lsp-server
   ```

   and then install the [OCaml
   Platform](https://marketplace.visualstudio.com/items?itemName=ocamllabs.ocaml-platform)
   from the Visual Studio Marketplace

# Assignment Workflow

Once everything is set up, working on an assignment will go roughly as
follows.

1. Sync with the course repository to get access to the assignment

2. Set the local switch to be the current active switch

   ```
   eval $(opam env)
   ```

3. Fill in your solutions, make sure to commit your changes frequently.
   *Note.* You will have to create your own files which satisfy the specification of the assignment.
   You can verify that you've set everything up correctly if `dune build` doesn't show any errors when run in `assigns/assignXX`

4. From within the assignment `lib` directory, frequently run `dune build`
   to type check your code

5. As you get close to completing the assignment, from within the
   assignment directory, run `dune test` to test your code against a
   small OUnit test suite. This collection of tests will include some
   (but not all) tests used for grading.

6. (Optional) Add your own OUnit tests to the `test` directory

7. Push your work to your private repository and submit your
   assignment via Gradescope

On Gradescope, you should see a message which says that your
assignment was accepted.  You will not see any other information
there.

## Important: Your code must build to receive credit

Please make sure that **your code and provided tests at least build**
(via `dune build` **in the course repository**) before submission.

This means that, even if you are only partially completing the
assignment, you may have to introduce skeleton code to the `lib`
directory.
