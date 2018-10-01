
class Song(object):


    def __init__(self, lyrics):
        self.lyrics = lyrics

    def sing_me_a_song(self):
        for line in self.lyrics:
            print(line)


happy_bday = song(["Python is good!",
                    "Programming is bad",
                    "Life is sad w/o Python"])

ayy_lmao = Song(["I have no idea what's I am doing.",
                 "Just following the instructions here tbh."])

happy_bday.sing_me_a_song

ayy_lmao.sing_me_a_song
