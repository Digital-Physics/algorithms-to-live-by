import redis
import requests

# Connect to Redis server
redis_client = redis.StrictRedis(host='localhost', port=6379, decode_responses=True)

def fetch_data_from_api(api_url):
    # Simulate fetching data from an API
    response = requests.get(api_url)
    return response.json() if response.status_code == 200 else None

def get_data_with_caching(api_url, cache_key, expiration_time=60):
    # Check if data is already in the cache
    cached_data = redis_client.get(cache_key)

    if cached_data is not None:
        # If data is in the cache, return it
        print("Data retrieved from cache")
        return cached_data
    else:
        # If data is not in the cache, fetch it from the API
        data = fetch_data_from_api(api_url)

        if data is not None:
            # Store the fetched data in the cache with a specified expiration time
            redis_client.setex(cache_key, expiration_time, str(data))

            print("Data fetched from API and stored in cache")
            return data
        else:
            print("Failed to fetch data from API")
            return None

# Example usage with JSONPlaceholder API
api_url = "https://jsonplaceholder.typicode.com/todos/1"
cache_key = "cached_data_key"

# Try to get data from the cache
result = get_data_with_caching(api_url, cache_key)

# If not in cache, it will fetch from API and store in cache for future use
print("Result:", result)
