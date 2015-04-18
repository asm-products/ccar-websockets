# ccar-websockets

<a href="https://assembly.com/ccar-websockets/bounties?utm_campaign=assemblage&utm_source=ccar-websockets&utm_medium=repo_badge"><img src="https://asm-badger.herokuapp.com/ccar-websockets/badges/tasks.svg" height="24px" alt="Open Tasks" /></a>

## Something realtime....

This is a product being built by the Assembly community. You can help push this idea forward by visiting [https://assembly.com/ccar-websockets](https://assembly.com/ccar-websockets).

### How Assembly Works

Assembly products are like open-source and made with contributions from the community. Assembly handles the boring stuff like hosting, support, financing, legal, etc. Once the product launches we collect the revenue and split the profits amongst the contributors.

Visit [https://assembly.com](https://assembly.com) to learn more.
=======


Compilers/libraries
================
The versions listed are the ones on my host. Most of the chnages will be backward compatible, so if you 
have downloaded a leter version, things will most likely work.
* ghci: > 7.6.3 && < 7.7
* cabal:  > 1.20 && < 1.21
Cabal setup may take some time and is not the prettiest in my experience. But eventually it does work well, so if you get stuck let me know.

* haxe : 3.0.0


* Haxe Libraries:
* mlib: [2.0.2]
* munit: [2.1.0]
* mconsole: [1.6.0]
* promhx: [1.0.16]
* hamcrest: [1.2.1]
* mcover: [2.1.1]


Installation of the yesod toolchain
====================================
* $ cabal sandbox init
* $ cabal install yesod-core --max-backjumps=-1 --reorder-goals --reinstall


Instructions to login to postgres
====================================
sudo -u postgres psql -d <dbname>

