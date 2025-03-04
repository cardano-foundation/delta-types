If you find a bug or you'd like to propose a feature, please feel free to raise an issue on our [issue tracker](https://github.com/cardano-foundation/delta-types/issues).

Please note we have a [code of conduct](CODE_OF_CONDUCT.md), please follow it in all your interactions with the project.

# Development

## Branches

We use [trunk-based development][trunk]: We merge features and improvements into the `main` branch frequently. If a large feature takes more than one pull request to develop, and is therefore incomplete, we hide it behind a feature flag. The `main` branch should be kept in a state where all checks pass, so that we can release it at any time.

  [trunk]: https://martinfowler.com/articles/branching-patterns.html#Trunk-basedDevelopment

## Continuous Integration (CI)

We use [continuous integration][ci]: Building artifacts, and testing artifacts is done automatically before changes are admitted to the `main` branch.

We use [Github Actions][gh] for automation.

  [gh]: https://github.com/features/actions
  [ci]: https://www.goodreads.com/book/show/17255186-the-phoenix-project
