import csv
from itertools import islice

size = 10000

with open('fer2013.csv') as csvfile:
    reader = csv.reader(csvfile)
    reader.next()

    with open('wfer2013.tab', 'wb') as out_file:
        writer = csv.writer(out_file, delimiter='\t')

        writer.writerow(['p%d' % i for i in xrange(48*48)] +
                        ['set-name', 'emotion'])
        writer.writerow(['c' for i in xrange(48*48)] +
                        ['string', 'd'])
        writer.writerow(['' for i in xrange(48*48)] +
                        ['', 'class'])
        for emotion, pixels, set_name in reader:
            pixels = map(lambda x: float(x) / 255.0, pixels.split())
            writer.writerow(pixels + [set_name.lower(), emotion])
