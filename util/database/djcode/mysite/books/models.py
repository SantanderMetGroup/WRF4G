from django.db import models


class Blog(models.Model):

    name=models.CharField(max_length=100)
    tagline=models.TextField()
    def __unicode__(self):
        return self.name

class Author(models.Model):
    name=models.CharField(max_length=50)
    email=models.EmailField()
    def __unicode__(self):
        return self.name

class Entry(models.Model):
    blog=models.ForeignKey(Blog)
    headline=models.CharField(max_length=255)
    body_text=models.TextField()
    pub_date=models.DateTimeField()
    authors=models.ManyToManyField(Author)
    def __unicode__(self):
        return self.headline


    

     
