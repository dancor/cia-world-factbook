download:
	cd html && cat ../nation-codes | xargs -I {} wget https://www.cia.gov/library/publications/the-world-factbook/geos/countrytemplate_{}.html
