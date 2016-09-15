# Install
First, ensure that you have the Haskell Platform and Haskell Stack installed. Then,
~~~
git clone "https://github.com/sejdm/smartWallpaper"
cd smartWallpaper
stack build
stack exec wallpaper-exe # to run the program
~~~

# Setup
Before running, edit the paths in the beginning of `./app/Main.hs` to include the paths to your org files and also to folders where you have icons (if you wish to have them). If you wish to have icons, you can look for the names that the files should have in `./src/Calendar.hs` and `./src/Weather.hs`.
