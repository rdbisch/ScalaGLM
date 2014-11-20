#!/usr/bin/python

#Convert a CSV into JSON for AvroReader 
# usage
#   $   convert_csv.py  avro_schema csv_file

import avro.schema
import json
import csv
import sys

mapping = {
        u"string": lambda x: x,
        u"int": lambda x: int(x),
        u"long": lambda x: long(x),
        u"double": lambda x: float(x)
}

with open(sys.argv[1], 'rb') as rawSchema:
    schema = avro.schema.parse(rawSchema.read())
    types = {}
    for k,v in schema.fields_dict.iteritems():
        types[k] = v.type.type

    with open(sys.argv[2], 'rb') as csvfile:
        reader = csv.DictReader(csvfile)
        for row in reader:
            result = {k: mapping[v](row[k]) for k, v in types.iteritems()}
            print json.dumps(result)
