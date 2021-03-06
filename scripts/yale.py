"""
Download & unzip

    http://vision.ucsd.edu/~iskwak/ExtYaleDatabase/Yale%20Face%20Database.htm

Run:

    python yale.py ../CroppedYale results_file.tab
"""
import re
import sys
import os
import csv
import random
from os import path

from PIL import Image

SUBJECTS_COUNT = 10
IMAGES_PER_SUBJECT = 30
TRAIN_SIZE = (SUBJECTS_COUNT * IMAGES_PER_SUBJECT) * 2 / 3
FINAL_SIZE = (26, 26)

filename_regexp = re.compile(
    r'yaleB(?P<subject>\d{2})_'
    r'P(?P<pose>00)'
    r'A(?P<azimut>(\+|\-)\d{3})'
    r'E(?P<elevation>(\+|\-)\d{2})'
    r'\.pgm'
)

fname = r'yaleB01_P00A+000E+20.pgm'

def get_files(subj_dir, images_per_subj):
    files = os.listdir(subj_dir)
    data = []
    for f in files:
        match = filename_regexp.match(f)
        if match:
            data.append([
                f,
                int(match.group('azimut')),
                int(match.group('elevation'))
            ])

    # Remove dark photos
    # data = filter(lambda x: abs(x[2]) < 50, data)
    # data = sorted(data, key=lambda x: abs(x[1]))
    random.shuffle(data)
    file_names = [x[0] for x in data[:images_per_subj]]
    return [(subj_dir, x) for x in file_names]

def write_images(file_name, rows, pixels_count):
    with open(file_name, 'wb') as result_file:
        writer = csv.writer(result_file, delimiter='\t')

        writer.writerow(['p%d' % i for i in xrange(pixels_count)] +
                        ['subject'])
        writer.writerow(['c' for i in xrange(pixels_count)] + ['d'])
        writer.writerow(['' for i in xrange(pixels_count)] + ['class'])
        for row in rows:
            writer.writerow(row)

def main(args):
    source_dir = args[0]
    target_dir = args[1]
    pixels_count = FINAL_SIZE[0] * FINAL_SIZE[1]
    subject_dirs = [path.join(source_dir, 'yaleB%.2d' % i)
                    for i in xrange(1, SUBJECTS_COUNT + 1)]
    subjects_filenames = map(lambda dir: get_files(dir, IMAGES_PER_SUBJECT), subject_dirs)

    rows = []

    for filenames in subjects_filenames:
        for subj_dir, filename in filenames:
            subject_id = int(re.search(r'\d{2}', subj_dir).group())
            file_path = path.join(subj_dir, filename)
            image = Image.open(file_path)
            resized = image.resize(FINAL_SIZE)
            image_data = resized.getdata()
            pixels = [image_data.getpixel((i, j)) / 255.
                      for i in range(FINAL_SIZE[0])
                      for j in range(FINAL_SIZE[1])]
            rows.append(pixels + [subject_id])

    random.shuffle(rows)
    train_rows, test_rows = rows[:TRAIN_SIZE], rows[TRAIN_SIZE:]
    train_file = path.join(target_dir, 'yale-train.tab')
    test_file = path.join(target_dir, 'yale-test.tab')
    write_images(train_file, train_rows, pixels_count)
    write_images(test_file, test_rows, pixels_count)

if __name__ == '__main__':
    main(sys.argv[1:])
