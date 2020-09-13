# http://www.imagemagick.org/discourse-server/viewtopic.php?p=48436#p48436
# http://www.imagemagick.org/discourse-server/viewtopic.php?p=115350#p115350

#convert 16.png 32.png 48.png 64.png c4.ico
#convert 64.png -define icon:auto-resize=64,48,32,16 c4.ico

convert icon.png -define icon:auto-resize=64,48,32,16 icon.ico
