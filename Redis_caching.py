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
api_url0 = "https://jsonplaceholder.typicode.com/todos/1"
api_url1 = "https://jsonplaceholder.typicode.com/todos/2"
api_url2 = "https://jsonplaceholder.typicode.com/todos/1" # same as first api call to test caching
cache_key0 = "cached_data_key0"
cache_key1 = "cached_data_key1"
# (two variables with same key was used to be explicit that these were two different calls)
cache_key2 = "cached_data_key0" # this should be retrieved from cache after cache_key0 call stores it 


# Try to get data from the cache
result0 = get_data_with_caching(api_url0, cache_key0)
result1 = get_data_with_caching(api_url1, cache_key1)
result2 = get_data_with_caching(api_url2, cache_key2)

# If not in cache, it will fetch from API and store in cache for future use
print("Result0:", result0)
print("Result1:", result1)
print("Result2:", result2)
