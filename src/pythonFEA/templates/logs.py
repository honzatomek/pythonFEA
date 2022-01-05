import logging

BASIC_FORMAT = logging.Formatter('%(message)s')
ADVANCED_FORMAT = logging.Formatter('%(asctime)6s *%(levelname).1s* %(message)s', datefmt='%H%M%S')

logging.basicConfig(level=logging.INFO, format='%(asctime)6s *%(levelname).1s* %(message)s', datefmt='%H%M%S')
