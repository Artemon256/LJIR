# LJIR
LiveJournal Image Reuploader

A small program which reuploads images from LiveJournal to Flickr. 

If you have the photos uploaded to a service which is banned in your country (Yandex.Fotki in Ukraine) or which is closed for some reason but you have your photos on your local disk - "upload-from-disk" branch is what you need. You just need to specify a link to the post and path to your folder with images in LJIR and hit "РАБОТАЙ" button. After a few minutes  you'll have all the images from the post reuploaded to Flickr.

The English version of LJIR is coming soon.

In order to protect your Flickr account from being spammed by avatars and other stuff, LJIR  ignores the photos sized less than 4kb.  If you want to change the minimal size of a file just set the prefered value in "config.ini". Attention! Flickr returns an error if LJIR tries to upload very small image. Be careful, prefered value is 4kb. 

Notice that photos' filenames have to be the same as in links in your posts.
