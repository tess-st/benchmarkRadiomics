{
 "cells": [
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {},
   "outputs": [],
   "source": [
    "#!/usr/bin/env python\n",
    "\n",
    "from __future__ import print_function\n",
    "\n",
    "import collections\n",
    "import csv\n",
    "import logging\n",
    "import os\n",
    "\n",
    "import SimpleITK as sitk\n",
    "\n",
    "import radiomics\n",
    "from radiomics import featureextractor\n",
    "\n",
    "\n",
    "def main():\n",
    "  outPath = r''\n",
    "\n",
    "  inputCSV = os.path.join(outPath, 'testCases.csv')\n",
    "  outputFilepath = os.path.join(outPath, 'radiomics_features.csv')\n",
    "  progress_filename = os.path.join(outPath, 'pyrad_log.txt')\n",
    "  params = os.path.join(outPath, 'exampleSettings', 'Params.yaml')\n",
    "\n",
    "  # Configure logging\n",
    "  rLogger = logging.getLogger('radiomics')\n",
    "\n",
    "  # Set logging level\n",
    "  # rLogger.setLevel(logging.INFO)  # Not needed, default log level of logger is INFO\n",
    "\n",
    "  # Create handler for writing to log file\n",
    "  handler = logging.FileHandler(filename=progress_filename, mode='w')\n",
    "  handler.setFormatter(logging.Formatter('%(levelname)s:%(name)s: %(message)s'))\n",
    "  rLogger.addHandler(handler)\n",
    "\n",
    "  # Initialize logging for batch log messages\n",
    "  logger = rLogger.getChild('batch')\n",
    "\n",
    "  # Set verbosity level for output to stderr (default level = WARNING)\n",
    "  radiomics.setVerbosity(logging.INFO)\n",
    "\n",
    "  logger.info('pyradiomics version: %s', radiomics.__version__)\n",
    "  logger.info('Loading CSV')\n",
    "\n",
    "  flists = []\n",
    "  try:\n",
    "    with open(inputCSV, 'r') as inFile:\n",
    "      cr = csv.DictReader(inFile, lineterminator='\\n')\n",
    "      flists = [row for row in cr]\n",
    "  except Exception:\n",
    "    logger.error('CSV READ FAILED', exc_info=True)\n",
    "\n",
    "  logger.info('Loading Done')\n",
    "  logger.info('Patients: %d', len(flists))\n",
    "\n",
    "  if os.path.isfile(params):\n",
    "    extractor = featureextractor.RadiomicsFeatureExtractor(params)\n",
    "  else:  # Parameter file not found, use hardcoded settings instead\n",
    "    settings = {}\n",
    "    settings['binWidth'] = 25\n",
    "    settings['resampledPixelSpacing'] = None  # [3,3,3]\n",
    "    settings['interpolator'] = sitk.sitkBSpline\n",
    "    settings['enableCExtensions'] = True\n",
    "\n",
    "    extractor = featureextractor.RadiomicsFeatureExtractor(**settings)\n",
    "    # extractor.enableInputImages(wavelet= {'level': 2})\n",
    "\n",
    "  logger.info('Enabled input images types: %s', extractor.enabledImagetypes)\n",
    "  logger.info('Enabled features: %s', extractor.enabledFeatures)\n",
    "  logger.info('Current settings: %s', extractor.settings)\n",
    "\n",
    "  headers = None\n",
    "\n",
    "  for idx, entry in enumerate(flists, start=1):\n",
    "\n",
    "    logger.info(\"(%d/%d) Processing Patient (Image: %s, Mask: %s)\", idx, len(flists), entry['Image'], entry['Mask'])\n",
    "\n",
    "    imageFilepath = entry['Image']\n",
    "    maskFilepath = entry['Mask']\n",
    "    label = entry.get('Label', None)\n",
    "\n",
    "    if str(label).isdigit():\n",
    "      label = int(label)\n",
    "    else:\n",
    "      label = None\n",
    "\n",
    "    if (imageFilepath is not None) and (maskFilepath is not None):\n",
    "      featureVector = collections.OrderedDict(entry)\n",
    "      featureVector['Image'] = os.path.basename(imageFilepath)\n",
    "      featureVector['Mask'] = os.path.basename(maskFilepath)\n",
    "\n",
    "      try:\n",
    "        featureVector.update(extractor.execute(imageFilepath, maskFilepath, label))\n",
    "\n",
    "        with open(outputFilepath, 'a') as outputFile:\n",
    "          writer = csv.writer(outputFile, lineterminator='\\n')\n",
    "          if headers is None:\n",
    "            headers = list(featureVector.keys())\n",
    "            writer.writerow(headers)\n",
    "\n",
    "          row = []\n",
    "          for h in headers:\n",
    "            row.append(featureVector.get(h, \"N/A\"))\n",
    "          writer.writerow(row)\n",
    "      except Exception:\n",
    "        logger.error('FEATURE EXTRACTION FAILED', exc_info=True)\n",
    "\n",
    "if __name__ == '__main__':\n",
    "  main()"
   ]
  }
 ],
 "metadata": {
  "kernelspec": {
   "display_name": "Python 3",
   "language": "python",
   "name": "python3"
  },
  "language_info": {
   "codemirror_mode": {
    "name": "ipython",
    "version": 3
   },
   "file_extension": ".py",
   "mimetype": "text/x-python",
   "name": "python",
   "nbconvert_exporter": "python",
   "pygments_lexer": "ipython3",
   "version": "3.8.10"
  }
 },
 "nbformat": 4,
 "nbformat_minor": 2
}