import csv
import json
import requests
import argparse
from fuzzywuzzy import fuzz

def is_similar(str1, str2):
    return fuzz.ratio(str1.lower(), str2.lower()) > 70  # Adjust the threshold as needed

def get_yelp_data(api_key, restaurant_name, city):
    url = 'https://api.yelp.com/v3/businesses/search'
    headers = {'Authorization': f'Bearer {api_key}'}
    params = {'term': restaurant_name, 'location': city}

    print(f"\nGetting data for restaurant: {restaurant_name} in {city}")
    response = requests.get(url, headers=headers, params=params)
    if response.status_code == 200:
        return response.json()
    else:
        print(f"Failed to retrieve data for restaurant: {restaurant_name}. Status code: {response.status_code}")
        print("Yelp API Request Details:")
        print(f"URL: {url}")
        print(f"Headers: {headers}")
        print(f"Parameters: {params}")
        print(f"Request: Response: {response.text}")
        return None

def parse_yelp_response(yelp_response, target_name, target_city):
    parsed_data = []
    for business in yelp_response.get('businesses', []):
        business_name = business.get('name', '')
        business_city = business.get('location', {}).get('city', '')

        if is_similar(business_name, target_name) and is_similar(business_city, target_city):
            restaurant_data = {
                'name': business_name,
                'categories': ', '.join([category['title'] for category in business.get('categories', [])]),
                'rating': business.get('rating', ''),
                'price': business.get('price', ''),
                'address': ', '.join(business.get('location', {}).get('display_address', [])),
                'phone': business.get('phone', ''),
                'latitude': business.get('coordinates', {}).get('latitude', ''),
                'longitude': business.get('coordinates', {}).get('longitude', '')
            }
            parsed_data.append(restaurant_data)
            print(f"Restaurant {business_name} is added.")

    if len(parsed_data) == 0:
      print(f"Restaurant {target_name} is not added.")

    return parsed_data

def save_to_json(data, output_file):
    print(f"Writing data to JSON file: {output_file}")
    with open(output_file, 'w') as json_file:
        json.dump(data, json_file, indent=2)

def save_to_csv(data, output_file):
    keys = data[0].keys()
    print(f"Writing data to CSV file: {output_file}")
    with open(output_file, 'w', newline='') as csv_file:
        writer = csv.DictWriter(csv_file, fieldnames=keys)
        writer.writeheader()
        writer.writerows(data)

def main():
    parser = argparse.ArgumentParser(description='Fetch restaurant data from Yelp API.')
    parser.add_argument('input_file', help='Input CSV file containing restaurant names')
    parser.add_argument('output_format', choices=['json', 'csv'], help='Output format (json or csv)')
    parser.add_argument('output_file', help='Output file name')
    parser.add_argument('api_key', help='Yelp API Bearer Token')

    args = parser.parse_args()

    print(f"\nReading restaurant names and city from CSV: {args.input_file}")

    with open(args.input_file, 'r', encoding='utf-8-sig') as csv_file:
        reader = csv.DictReader(csv_file)
        print(reader.fieldnames)
        if 'restaurant_name' not in reader.fieldnames or 'city' not in reader.fieldnames:
            print("Error: CSV file must have columns 'restaurant_name' and 'city'.")
        restaurant_data_list = [{'name': row['restaurant_name'], 'city': row['city']} for row in reader]

    yelp_data_list = []
    for restaurant_data in restaurant_data_list:
        yelp_response = get_yelp_data(args.api_key, restaurant_data['name'], restaurant_data['city'])
        if yelp_response:
            parsed_data = parse_yelp_response(yelp_response, restaurant_data['name'], restaurant_data['city'])
            yelp_data_list.extend(parsed_data)

    if args.output_format == 'json':
        save_to_json(yelp_data_list, args.output_file)
    elif args.output_format == 'csv':
        save_to_csv(yelp_data_list, args.output_file)

if __name__ == "__main__":
    main()
