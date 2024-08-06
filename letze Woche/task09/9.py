import pandas as pd

# Funktion zum Laden und Anzeigen der ersten 10 Zeilen eines DataFrames
def load_and_preview_csv(file_path):
    df = pd.read_csv(file_path)
    print(f"Erste 10 Zeilen von {file_path}:")
    print(df.head(10))
    return df

# Funktion zur Berechnung der Anzahl und des Prozentsatzes von männlichen und weiblichen Erstsemestern
def calculate_gender_distribution(df):
    total_students = len(df)
    male_students = len(df[df['Gender'] == 'M'])
    female_students = len(df[df['Gender'] == 'F'])
    male_percentage = (male_students / total_students) * 100
    female_percentage = (female_students / total_students) * 100
    print(f"Total Students: {total_students}")
    print(f"Male: {male_students} ({male_percentage:.2f}%)")
    print(f"Female: {female_students} ({female_percentage:.2f}%)")

# Funktion zur Gruppierung nach Häusern und Ausgabe der Gruppen
def group_by_house(df):
    grouped_by_house = df.groupby('House')
    for house, group in grouped_by_house:
        print(f"{house}: {len(group)}")
    most_students_house = grouped_by_house.size().idxmax()
    print(f"House with the most students: {most_students_house}")

# Funktion zum Zusammenfassen von Vor- und Nachnamen zu einer Spalte
def combine_names(df):
    df = df.assign(Name=df['FirstName'] + ' ' + df['LastName']).drop(columns=['FirstName', 'LastName'])
    return df

# Funktion zum Extrahieren der Benutzernamen aus E-Mails
def extract_usernames(df):
    df['Username'] = df['Email'].apply(lambda x: x.split('@')[0])
    return df

# Pfade zu den Dateien (passen Sie die Pfade an Ihren lokalen Dateispeicherort an)
erstsemester_path = 'ErstSemester.csv'
studierenden_path = 'StudierendenDaten.csv'

# Laden und Voransicht der Erstsemester-Daten
erstsemester_df = load_and_preview_csv(erstsemester_path)

# Berechnung der Geschlechterverteilung
calculate_gender_distribution(erstsemester_df)

# Gruppierung nach Häusern
group_by_house(erstsemester_df)

# Zusammenfassen der Namen und Extrahieren der Benutzernamen
erstsemester_df = combine_names(erstsemester_df)
erstsemester_df = extract_usernames(erstsemester_df)

# Laden und Voransicht der Studierendendaten
studierenden_df = load_and_preview_csv(studierenden_path)

# Zusammenfassen der Namen und Extrahieren der Benutzernamen für Studierendendaten
studierenden_df = combine_names(studierenden_df)
studierenden_df = extract_usernames(studierenden_df)

# Funktion zum Separieren von Strings anhand eines Separators
def separate(strings, separator=","):
    return [string.split(separator) for string in strings]

# Benutzername aus der E-Mail extrahieren
studierenden_df['Username'] = studierenden_df['Email'].apply(lambda x: separate([x], '@')[0][0])

# Erklärung: Der Parameter axis bestimmt, entlang welcher Achse die Arrays zusammengefügt werden:
# axis=0: Zeilenweise (vertikal), axis=1: Spaltenweise (horizontal)

# Vertikal zusammenfügen
combined_df_axis0 = pd.concat([studierenden_df, erstsemester_df], axis=0)

# Horizontal zusammenfügen
combined_df_axis1 = pd.concat([studierenden_df, erstsemester_df], axis=1)

# Ergebnisse der Zusammenfügung erklären
print("Vertikales Zusammenfügen erweitert die Zeilenanzahl, horizontales Zusammenfügen erweitert die Spaltenanzahl.")
print("Weiterarbeiten sollte man mit der vertikalen Zusammenfügung (axis=0), um die Daten kohärent zu halten.")

# Ohne Neustart des Index (ignorieren)
combined_df_no_ignore = pd.concat([studierenden_df, erstsemester_df], axis=0, ignore_index=False)

# Mit Neustart des Index
combined_df_ignore = pd.concat([studierenden_df, erstsemester_df], axis=0, ignore_index=True)

# Erklärung des Unterschieds
print("Erklärung: ignore_index=True startet den Index neu, was nützlich ist, wenn die Indizes der ursprünglichen DataFrames nicht relevant sind.")
