import imaplib
import keyring
obj = imaplib.IMAP4_SSL('imap.gmail.com','993')
obj.login('david.buchmann@gmail.com',
          keyring.get_password('gmail', 'david.buchmann@gmail.com'))
obj.select()
obj.search(None,'UnSeen')
print len(obj.search(None, 'UnSeen')[1][0].split())
