# Install
First, ensure that you have the Haskell Platform and Haskell Stack installed. Then,
~~~
git clone "https://github.com/sejdm/smartWallpaper"
cd smartWallpaper
stack build
~~~

You will need imagemagick's `display` command. Imagemagick should be available in most repositories, and on Ubuntu can be installed by:

~~~
sudo apt-get install imagemagick
~~~

# Setup
Before running, edit the paths in the beginning of `./app/Main.hs` to include the paths to your org files and also to folders where you have icons (if you wish to have them). If you wish to have icons, you can look for the names that the files should have in `./src/Calendar.hs` and `./src/Weather.hs`.

# Running the program
You can run the program by executing the following from within the clone of your repository:

~~~
stack exec wallpaper-exe
~~~
