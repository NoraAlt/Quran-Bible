# Quran-Bible
Text Mining and Similarity Measures of the Quran and the Bible

The goal of this project is to use programming and statistical tools to analyse unstructured data for information of interest embedded within the Bible and the Quran texts.

In Datasets folder you will find the following training and test datasets, used in the experiements, in CSV format:

1- Bible Cross References (BCR)
Data fields description:
¥	id – the auto incremental id of a verse pair.
¥	Book1, Book2 – the name of a Bible book.
¥	BookID1, BookID2 – unique ids of each Bible book.
¥	ChapterID1, ChapterID2 - unique ids of each chapter in a Bible book.
¥	VerseID1, VerseID2 - unique ids of each verse within a chapter in a Bible book.
¥	Text1, Text2 - the full text of each verse.
¥	is_similar – the target variable, set to 1 if Text1 and Text2 have essentially the same meaning, and 0 otherwise.

2- Quranic verses from QurSim dataset with relatedness degree 2 (Q2)
3- Quranic verses from QurSim dataset with relatedness degree 1 or 0 (Q1)
Data fields description:
¥	id – the auto incremental id of a verse pair.
¥	SuraID1, SuraID2 - unique id of a surah (chapter) in the Quran.
¥	VerseID1, VerseID2 - unique ids of each verse within a surah in the Quran.
¥	Text1, Text2 - the full text of each verse.
¥ is_similar – the target variable, set to 1 if Text1 and Text2 have essentially the same meaning, and 0 otherwise.


4- Pairs of verses from the Quran and the Bible (PQB)
Data fields description:
¥	id – the auto incremental id of a verse pair.
¥	Q.SuraID – unique id of a surah (chapter) in the Quran.
¥	Q.VerseID – unique ids of each verse within a surah in the Quran.
¥	Q.Text – the full text of target verse in the Quran.
¥	B.BookName – the name of a Bible book.
¥	B.BookID – unique ids of each Bible book.
¥	B.ChapterID – unique id of a chapter in the Bible.
¥	B.VerseID – unique ids of each verse within a chapter in in a Bible book.
¥	B.Text – the full text of target verse in the Bible.
¥	is_similar – the target variable, set to 1 if Text1 and Text2 have essentially the same meaning, and 0 otherwise.
